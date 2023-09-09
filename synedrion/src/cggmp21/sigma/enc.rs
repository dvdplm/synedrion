//! Paillier encryption in range ($\Pi^{enc}$, Section 6.1, Fig. 14)

use rand_core::CryptoRngCore;
use serde::{Deserialize, Serialize};

use super::super::SchemeParams;
use crate::paillier::{
    Ciphertext, PaillierParams, PublicKeyPaillierPrecomputed, RPCommitment, RPParamsMod,
    SecretKeyPaillierPrecomputed,
};
use crate::tools::hashing::{Chain, Hash, Hashable};
use crate::uint::{NonZero, Retrieve, Signed, UintModLike};

#[derive(Clone, Serialize, Deserialize)]
pub(crate) struct EncProof<P: SchemeParams> {
    cap_s: RPCommitment<P::Paillier>,
    cap_a: Ciphertext<P::Paillier>,
    cap_c: RPCommitment<P::Paillier>,
    z1: Signed<<P::Paillier as PaillierParams>::DoubleUint>,
    z2: <P::Paillier as PaillierParams>::DoubleUint,
    z3: Signed<<P::Paillier as PaillierParams>::QuadUint>,
}

impl<P: SchemeParams> EncProof<P> {
    pub fn random(
        rng: &mut impl CryptoRngCore,
        secret: &Signed<<P::Paillier as PaillierParams>::DoubleUint>, // `k`
        randomizer: &<P::Paillier as PaillierParams>::DoubleUint,     // `\rho`
        sk: &SecretKeyPaillierPrecomputed<P::Paillier>,               // `N_0`
        aux_rp: &RPParamsMod<P::Paillier>,                            // $\hat{N}$, $s$, $t$
        aux: &impl Hashable, // CHECK: used to derive `\hat{N}, s, t`
    ) -> Self {
        let pk = sk.public_key();

        let mut aux_rng = Hash::new_with_dst(b"P_enc").chain(aux).finalize_to_rng();

        let hat_cap_n = &aux_rp.public_key().modulus_nonzero(); // $\hat{N}$

        // Non-interactive challenge ($e$)
        let challenge =
            Signed::random_bounded(&mut aux_rng, &NonZero::new(P::CURVE_ORDER).unwrap());

        // TODO: check that `bound` and `bound_eps` do not overflow the DoubleUint
        // TODO: check that `secret` is within `+- 2^bound`

        // \alpha <-- +- 2^{\ell + \eps}
        // CHECK: should we instead sample in range $+- 2^{\ell + \eps} - q 2^\ell$?
        // This will ensure that the range check on the prover side will pass.
        let alpha = Signed::random_bounded_bits(rng, P::L_BOUND + P::EPS_BOUND);

        // \mu <-- (+- 2^\ell) * \hat{N}
        let mu = Signed::random_bounded_bits_scaled(rng, P::L_BOUND, hat_cap_n);

        // TODO: use `Ciphertext::randomizer()` - but we will need a variation returning a modulo
        // representation.
        // r <-- Z^*_N (N is the modulus of `pk`)
        let r = pk.random_invertible_group_elem(rng);

        // \gamma <-- (+- 2^{\ell + \eps}) * \hat{N}
        let gamma = Signed::random_bounded_bits_scaled(rng, P::L_BOUND + P::EPS_BOUND, hat_cap_n);

        // S = s^k * t^\mu \mod \hat{N}
        let cap_s = aux_rp.commit(&mu, secret).retrieve();

        // A = (1 + N_0)^\alpha * r^N_0 == encrypt(\alpha, r)
        let cap_a = Ciphertext::new_with_randomizer_signed(pk, &alpha, &r.retrieve());

        // C = s^\alpha * t^\gamma \mod \hat{N}
        let cap_c = aux_rp.commit(&gamma, &alpha).retrieve();

        // z_1 = \alpha + e k
        // In the proof it will be checked that $z1 \in +- 2^{\ell + \eps}$,
        // so it should fit into DoubleUint.
        let z1 = alpha + challenge * *secret;

        // TODO: make a `pow_mod_signed()` method to hide this giant type?
        // z_2 = r * \rho^e mod N_0
        let randomizer_mod = <P::Paillier as PaillierParams>::DoubleUintMod::new(
            randomizer,
            pk.precomputed_modulus(),
        );
        let z2 = (r * randomizer_mod.pow_signed(&challenge)).retrieve();

        // z_3 = \gamma + e * \mu
        let challenge_wide: Signed<<P::Paillier as PaillierParams>::QuadUint> =
            challenge.into_wide();
        let z3 = gamma + mu * challenge_wide;

        Self {
            cap_s,
            cap_a,
            cap_c,
            z1,
            z2,
            z3,
        }
    }

    pub fn verify(
        &self,
        pk: &PublicKeyPaillierPrecomputed<P::Paillier>, // `N_0`
        ciphertext: &Ciphertext<P::Paillier>,           // `K`
        aux_rp: &RPParamsMod<P::Paillier>,              // $s$, $t$
        aux: &impl Hashable,                            // CHECK: used to derive `\hat{N}, s, t`
    ) -> bool {
        let mut aux_rng = Hash::new_with_dst(b"P_enc").chain(aux).finalize_to_rng();

        // Non-interactive challenge ($e$)
        let challenge = Signed::<<P::Paillier as PaillierParams>::DoubleUint>::random_bounded(
            &mut aux_rng,
            &NonZero::new(P::CURVE_ORDER).unwrap(),
        );

        // Range check
        if !self.z1.in_range_bits(P::L_BOUND + P::EPS_BOUND) {
            return false;
        }

        // Check that $encrypt_{N_0}(z1, z2) == A (+) K (*) e$
        let c = Ciphertext::new_with_randomizer_signed(pk, &self.z1, &self.z2);

        if c != self
            .cap_a
            .homomorphic_add(pk, &ciphertext.homomorphic_mul(pk, &challenge))
        {
            return false;
        }

        // Check that $s^{z_1} t^{z_3} == C S^e \mod \hat{N}$
        let cap_c_mod = self.cap_c.to_mod(aux_rp.public_key());
        let cap_s_mod = self.cap_s.to_mod(aux_rp.public_key());
        if aux_rp.commit(&self.z3, &self.z1) != &cap_c_mod * &cap_s_mod.pow_signed(&challenge) {
            return false;
        }

        true
    }
}

#[cfg(test)]
mod tests {
    use rand_core::OsRng;

    use super::EncProof;
    use crate::cggmp21::{SchemeParams, TestParams};
    use crate::paillier::{Ciphertext, RPParamsMod, SecretKeyPaillier};
    use crate::uint::Signed;

    #[test]
    fn prove_and_verify() {
        type Params = TestParams;
        type Paillier = <Params as SchemeParams>::Paillier;

        let sk = SecretKeyPaillier::<Paillier>::random(&mut OsRng).to_precomputed();
        let pk = sk.public_key();

        let aux_sk = SecretKeyPaillier::<Paillier>::random(&mut OsRng).to_precomputed();
        let aux_rp = RPParamsMod::random(&mut OsRng, &aux_sk);

        let aux: &[u8] = b"abcde";

        let secret = Signed::random_bounded_bits(&mut OsRng, Params::L_BOUND);
        let randomizer = Ciphertext::<Paillier>::randomizer(&mut OsRng, pk);
        let ciphertext = Ciphertext::new_with_randomizer_signed(pk, &secret, &randomizer);

        let proof =
            EncProof::<Params>::random(&mut OsRng, &secret, &randomizer, &sk, &aux_rp, &aux);
        assert!(proof.verify(pk, &ciphertext, &aux_rp, &aux));
    }
}
