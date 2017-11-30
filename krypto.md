Cryptographic Primitives
========================

Here we implement the various cryptographic primitives needed for IELE.

```{.k .uiuck .rvk}
module KRYPTO
    imports STRING-SYNTAX
```

-   `Keccak256` takes a string and returns a 64-character hex-encoded string of the 32-byte keccak256 hash of the string.
-   `Sha256' takes a String and returns a 64-character hex-encoded string of the 32-byte SHA2-256 hash of the string.
-   `RipEmd160' takes a String and returns a 40-character hex-encoded string of the 20-byte RIPEMD160 hash of the string.
-   `ECDSARecover` takes a 32-character byte string of a message, v, r, s of the signed message and returns the 64-character public key used to sign the message.
    See [this StackOverflow post](https://ethereum.stackexchange.com/questions/15766/what-does-v-r-s-in-eth-gettransactionbyhash-mean) for some information about v, r, and s.
-   `BN128Add` takes two points on the G1 curve of the Barreto-Naehrig elliptic curve and adds them together.
-   `BN128Mul` takes a point on the G1 curve of the Barreto-Naehrig elliptic curve and multiplies it against a scalar.
-   `BN128AtePairing` computes the Ate pairing of a G1 point and a G2 point, and is used to perform computations relating to zero knowledge proofs.

```{.k .uiuck .rvk}
    syntax String ::= Keccak256 ( String )                            [function, hook(KRYPTO.keccak256)]
                    | ECDSARecover ( String , Int , String , String ) [function, hook(KRYPTO.ecdsaRecover)]
                    | Sha256 ( String )                               [function, hook(KRYPTO.sha256)]
                    | RipEmd160 ( String )                            [function, hook(KRYPTO.ripemd160)]

    syntax G1Point ::= "(" Int "," Int ")"
    syntax G2Point ::= "(" Int "x" Int "," Int "x" Int ")"
    syntax G1Point ::= BN128Add(G1Point, G1Point) [function, hook(KRYPTO.bn128add)]
                     | BN128Mul(G1Point, Int) [function, hook(KRYPTO.bn128mul)]

    syntax Bool ::= BN128AtePairing(List, List) [function, hook(KRYPTO.bn128ate)]

    syntax Bool ::= isValidPoint(G1Point) [function, hook(KRYPTO.bn128valid)]
                  | isValidPoint(G2Point) [function, klabel(isValidG2Point), hook(KRYPTO.bn128g2valid)]
endmodule
```
