//! Paillier multiplication ($\Pi^{mul}$, Section C.6, Fig. 29)

use rand_core::CryptoRngCore;
use serde::{Deserialize, Serialize};

use super::super::SchemeParams;
use crate::paillier::{
    Ciphertext, CiphertextMod, PaillierParams, PublicKeyPaillierPrecomputed, Randomizer,
    RandomizerMod,
};
use crate::tools::hashing::{Chain, Hashable, XofHasher};
use crate::uint::{Bounded, Retrieve, Signed};

const HASH_TAG: &[u8] = b"P_mul";

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct MulProof<P: SchemeParams> {
    e: Signed<<P::Paillier as PaillierParams>::Uint>,
    cap_a: Ciphertext<P::Paillier>,
    cap_b: Ciphertext<P::Paillier>,
    z: Signed<<P::Paillier as PaillierParams>::WideUint>,
    u: Randomizer<P::Paillier>,
    v: Randomizer<P::Paillier>,
}

/**
ZK proof: Paillier multiplication.

Secret inputs:
- $x$ (technically any integer since it will be implicitly reduced modulo $q$ or $\phi(N)$,
  but we limit its size to `Uint` since that's what we use in this library),
- $\rho_x$, a Paillier randomizer for the public key $N$,
- $\rho$, a Paillier randomizer for the public key $N$.

Public inputs:
- Paillier public key $N$,
- Paillier ciphertext $X = enc(x, \rho_x)$,
- Paillier ciphertext $Y$ encrypted with $N$,
- Paillier ciphertext $C = (Y (*) x) * \rho^N \mod N^2$,
- Setup parameters ($\hat{N}$, $s$, $t$).
*/
impl<P: SchemeParams> MulProof<P> {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        rng: &mut impl CryptoRngCore,
        x: &Signed<<P::Paillier as PaillierParams>::Uint>,
        rho_x: &RandomizerMod<P::Paillier>,
        rho: &RandomizerMod<P::Paillier>,
        pk: &PublicKeyPaillierPrecomputed<P::Paillier>,
        cap_x: &CiphertextMod<P::Paillier>,
        cap_y: &CiphertextMod<P::Paillier>,
        cap_c: &CiphertextMod<P::Paillier>,
        aux: &impl Hashable,
    ) -> Self {
        assert_eq!(cap_x.public_key(), pk);
        assert_eq!(cap_y.public_key(), pk);
        assert_eq!(cap_c.public_key(), pk);

        let alpha_mod = pk.random_invertible_group_elem(rng);
        let r_mod = RandomizerMod::random(rng, pk);
        let s_mod = RandomizerMod::random(rng, pk);

        let alpha = Bounded::new(
            alpha_mod.retrieve(),
            <P::Paillier as PaillierParams>::MODULUS_BITS as u32,
        )
        .unwrap();
        let r = r_mod.retrieve();
        let s = s_mod.retrieve();

        let cap_a = (cap_y * alpha).mul_randomizer(&r).retrieve();
        let cap_b = CiphertextMod::new_with_randomizer(pk, alpha.as_ref(), &s).retrieve();

        let mut reader = XofHasher::new_with_dst(HASH_TAG)
            // commitments
            .chain(&cap_a)
            .chain(&cap_b)
            // public parameters
            .chain(pk.as_minimal())
            .chain(&cap_x.retrieve())
            .chain(&cap_y.retrieve())
            .chain(&cap_c.retrieve())
            .chain(aux)
            .finalize_to_reader();

        // Non-interactive challenge
        let e = Signed::from_xof_reader_bounded(&mut reader, &P::CURVE_ORDER);

        let z = alpha.into_wide().into_signed().unwrap() + e.mul_wide(x);
        let u = (r_mod * rho.pow_signed_vartime(&e)).retrieve();
        let v = (s_mod * rho_x.pow_signed_vartime(&e)).retrieve();

        Self {
            e,
            cap_a,
            cap_b,
            z,
            u,
            v,
        }
    }

    pub fn verify(
        &self,
        pk: &PublicKeyPaillierPrecomputed<P::Paillier>,
        cap_x: &CiphertextMod<P::Paillier>,
        cap_y: &CiphertextMod<P::Paillier>,
        cap_c: &CiphertextMod<P::Paillier>,
        aux: &impl Hashable,
    ) -> bool {
        assert_eq!(cap_x.public_key(), pk);
        assert_eq!(cap_y.public_key(), pk);
        assert_eq!(cap_c.public_key(), pk);

        let mut reader = XofHasher::new_with_dst(HASH_TAG)
            // commitments
            .chain(&self.cap_a)
            .chain(&self.cap_b)
            // public parameters
            .chain(pk.as_minimal())
            .chain(&cap_x.retrieve())
            .chain(&cap_y.retrieve())
            .chain(&cap_c.retrieve())
            .chain(aux)
            .finalize_to_reader();

        // Non-interactive challenge
        let e = Signed::from_xof_reader_bounded(&mut reader, &P::CURVE_ORDER);

        if e != self.e {
            return false;
        }

        // Y^z u^N = A * C^e \mod N^2
        if cap_y.homomorphic_mul_wide(&self.z).mul_randomizer(&self.u)
            != self.cap_a.to_mod(pk) + cap_c * e
        {
            return false;
        }

        // enc(z, v) == B * X^e \mod N^2
        // (Note: typo in the paper, it uses `c` and not `v` here)
        if CiphertextMod::new_with_randomizer_wide(pk, &self.z, &self.v)
            != self.cap_b.to_mod(pk) + cap_x * e
        {
            return false;
        }

        true
    }
}

#[cfg(test)]
mod tests {
    use rand_core::OsRng;

    use super::MulProof;
    use crate::cggmp21::{SchemeParams, TestParams};
    use crate::paillier::{CiphertextMod, RandomizerMod, SecretKeyPaillier};
    use crate::uint::Signed;

    #[test]
    fn prove_and_verify() {
        type Params = TestParams;
        type Paillier = <Params as SchemeParams>::Paillier;

        let sk = SecretKeyPaillier::<Paillier>::random(&mut OsRng).to_precomputed();
        let pk = sk.public_key();

        let aux: &[u8] = b"abcde";

        let x = Signed::random_bounded_bits(&mut OsRng, Params::L_BOUND);
        let y = Signed::random_bounded_bits(&mut OsRng, Params::L_BOUND);
        let rho_x = RandomizerMod::random(&mut OsRng, pk);
        let rho = RandomizerMod::random(&mut OsRng, pk);

        let cap_x = CiphertextMod::new_with_randomizer_signed(pk, &x, &rho_x.retrieve());
        let cap_y = CiphertextMod::new_signed(&mut OsRng, pk, &y);
        let cap_c = (&cap_y * x).mul_randomizer(&rho.retrieve());

        let proof = MulProof::<Params>::new(
            &mut OsRng, &x, &rho_x, &rho, pk, &cap_x, &cap_y, &cap_c, &aux,
        );
        assert!(proof.verify(pk, &cap_x, &cap_y, &cap_c, &aux));
    }
}
