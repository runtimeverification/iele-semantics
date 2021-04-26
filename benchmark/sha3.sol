pragma solidity ^0.4.23;

contract Sha3 {
  uint64 constant RC0 = 0x0000000000000001; 
  uint64 constant RC1 = 0x0000000000008082;
  uint64 constant RC2 = 0x800000000000808a;
  uint64 constant RC3 = 0x8000000080008000;
  uint64 constant RC4 = 0x000000000000808b;
  uint64 constant RC5 = 0x0000000080000001;
  uint64 constant RC6 = 0x8000000080008081;
  uint64 constant RC7 = 0x8000000000008009;
  uint64 constant RC8 = 0x000000000000008a;
  uint64 constant RC9 = 0x0000000000000088;
  uint64 constant RC10 = 0x0000000080008009;
  uint64 constant RC11 = 0x000000008000000a;
  uint64 constant RC12 = 0x000000008000808b;
  uint64 constant RC13 = 0x800000000000008b;
  uint64 constant RC14 = 0x8000000000008089;
  uint64 constant RC15 = 0x8000000000008003;
  uint64 constant RC16 = 0x8000000000008002;
  uint64 constant RC17 = 0x8000000000000080;
  uint64 constant RC18 = 0x000000000000800a;
  uint64 constant RC19 = 0x800000008000000a;
  uint64 constant RC20 = 0x8000000080008081;
  uint64 constant RC21 = 0x8000000000008080;
  uint64 constant RC22 = 0x0000000080000001;
  uint64 constant RC23 = 0x8000000080008008;

  uint64 constant r00 = 0;
  uint64 constant r10 = 1;
  uint64 constant r20 = 62;
  uint64 constant r30 = 28;
  uint64 constant r40 = 27;

  uint64 constant r01 = 36;
  uint64 constant r11 = 44;
  uint64 constant r21 = 6;
  uint64 constant r31 = 55;
  uint64 constant r41 = 20;

  uint64 constant r02 = 3;
  uint64 constant r12 = 10;
  uint64 constant r22 = 43;
  uint64 constant r32 = 25;
  uint64 constant r42 = 39;

  uint64 constant r03 = 41;
  uint64 constant r13 = 45;
  uint64 constant r23 = 15;
  uint64 constant r33 = 21;
  uint64 constant r43 = 8;

  uint64 constant r04 = 18;
  uint64 constant r14 = 2;
  uint64 constant r24 = 61;
  uint64 constant r34 = 56;
  uint64 constant r44 = 14;

  function keccak_f(uint64[5][5] A) private pure returns (uint64[5][5]) {
    A = Round(A, RC0);
    A = Round(A, RC1);
    A = Round(A, RC2);
    A = Round(A, RC3);
    A = Round(A, RC4);
    A = Round(A, RC5);
    A = Round(A, RC6);
    A = Round(A, RC7);
    A = Round(A, RC8);
    A = Round(A, RC9);
    A = Round(A, RC10);
    A = Round(A, RC11);
    A = Round(A, RC12);
    A = Round(A, RC13);
    A = Round(A, RC14);
    A = Round(A, RC15);
    A = Round(A, RC16);
    A = Round(A, RC17);
    A = Round(A, RC18);
    A = Round(A, RC19);
    A = Round(A, RC20);
    A = Round(A, RC21);
    A = Round(A, RC22);
    A = Round(A, RC23);
    return A;
  }

  function rot(uint64 x, int n) private pure returns (uint64) {
    return (x << n) | (x >> (64 - n));
  }

  function Round(uint64[5][5] A, uint64 RCi) private pure returns (uint64[5][5]) {
    uint64[5] memory C;
    uint64[5] memory D;

    C[0] = A[0][0] ^ A[0][1] ^ A[0][2] ^ A[0][3] ^ A[0][4];
    C[1] = A[1][0] ^ A[1][1] ^ A[1][2] ^ A[1][3] ^ A[1][4];
    C[2] = A[2][0] ^ A[2][1] ^ A[2][2] ^ A[2][3] ^ A[2][4];
    C[3] = A[3][0] ^ A[3][1] ^ A[3][2] ^ A[3][3] ^ A[3][4];
    C[4] = A[4][0] ^ A[4][1] ^ A[4][2] ^ A[4][3] ^ A[4][4];

    D[0] = C[4] ^ rot(C[1],1);
    D[1] = C[0] ^ rot(C[2],1);
    D[2] = C[1] ^ rot(C[3],1);
    D[3] = C[2] ^ rot(C[4],1);
    D[4] = C[3] ^ rot(C[0],1);

    A[0][0] = A[0][0] ^ D[0];
    A[0][1] = A[0][1] ^ D[0];
    A[0][2] = A[0][2] ^ D[0];
    A[0][3] = A[0][3] ^ D[0];
    A[0][4] = A[0][4] ^ D[0];
    A[1][0] = A[1][0] ^ D[1];
    A[1][1] = A[1][1] ^ D[1];
    A[1][2] = A[1][2] ^ D[1];
    A[1][3] = A[1][3] ^ D[1];
    A[1][4] = A[1][4] ^ D[1];
    A[2][0] = A[2][0] ^ D[2];
    A[2][1] = A[2][1] ^ D[2];
    A[2][2] = A[2][2] ^ D[2];
    A[2][3] = A[2][3] ^ D[2];
    A[2][4] = A[2][4] ^ D[2];
    A[3][0] = A[3][0] ^ D[3];
    A[3][1] = A[3][1] ^ D[3];
    A[3][2] = A[3][2] ^ D[3];
    A[3][3] = A[3][3] ^ D[3];
    A[3][4] = A[3][4] ^ D[3];
    A[4][0] = A[4][0] ^ D[4];
    A[4][1] = A[4][1] ^ D[4];
    A[4][2] = A[4][2] ^ D[4];
    A[4][3] = A[4][3] ^ D[4];
    A[4][4] = A[4][4] ^ D[4];

    uint64[5][5] memory B;

    B[0][0] = rot(A[0][0], r00);
    B[1][3] = rot(A[0][1], r01);
    B[2][1] = rot(A[0][2], r02);
    B[3][4] = rot(A[0][3], r03);
    B[4][2] = rot(A[0][4], r04);
    B[0][2] = rot(A[1][0], r10);
    B[1][0] = rot(A[1][1], r11);
    B[2][3] = rot(A[1][2], r12);
    B[3][1] = rot(A[1][3], r13);
    B[4][4] = rot(A[1][4], r14);
    B[0][4] = rot(A[2][0], r20);
    B[1][2] = rot(A[2][1], r21);
    B[2][0] = rot(A[2][2], r22);
    B[3][3] = rot(A[2][3], r23);
    B[4][1] = rot(A[2][4], r24);
    B[0][1] = rot(A[3][0], r30);
    B[1][4] = rot(A[3][1], r31);
    B[2][2] = rot(A[3][2], r32);
    B[3][0] = rot(A[3][3], r33);
    B[4][3] = rot(A[3][4], r34);
    B[0][3] = rot(A[4][0], r40);
    B[1][1] = rot(A[4][1], r41);
    B[2][4] = rot(A[4][2], r42);
    B[3][2] = rot(A[4][3], r43);
    B[4][0] = rot(A[4][4], r44);

    A[0][0] = B[0][0] ^ ((~B[1][0]) & B[2][0]);
    A[0][1] = B[0][1] ^ ((~B[1][1]) & B[2][1]);
    A[0][2] = B[0][2] ^ ((~B[1][2]) & B[2][2]);
    A[0][3] = B[0][3] ^ ((~B[1][3]) & B[2][3]);
    A[0][4] = B[0][4] ^ ((~B[1][4]) & B[2][4]);
    A[1][0] = B[1][0] ^ ((~B[2][0]) & B[3][0]);
    A[1][1] = B[1][1] ^ ((~B[2][1]) & B[3][1]);
    A[1][2] = B[1][2] ^ ((~B[2][2]) & B[3][2]);
    A[1][3] = B[1][3] ^ ((~B[2][3]) & B[3][3]);
    A[1][4] = B[1][4] ^ ((~B[2][4]) & B[3][4]);
    A[2][0] = B[2][0] ^ ((~B[3][0]) & B[4][0]);
    A[2][1] = B[2][1] ^ ((~B[3][1]) & B[4][1]);
    A[2][2] = B[2][2] ^ ((~B[3][2]) & B[4][2]);
    A[2][3] = B[2][3] ^ ((~B[3][3]) & B[4][3]);
    A[2][4] = B[2][4] ^ ((~B[3][4]) & B[4][4]);
    A[3][0] = B[3][0] ^ ((~B[4][0]) & B[0][0]);
    A[3][1] = B[3][1] ^ ((~B[4][1]) & B[0][1]);
    A[3][2] = B[3][2] ^ ((~B[4][2]) & B[0][2]);
    A[3][3] = B[3][3] ^ ((~B[4][3]) & B[0][3]);
    A[3][4] = B[3][4] ^ ((~B[4][4]) & B[0][4]);
    A[4][0] = B[4][0] ^ ((~B[0][0]) & B[1][0]);
    A[4][1] = B[4][1] ^ ((~B[0][1]) & B[1][1]);
    A[4][2] = B[4][2] ^ ((~B[0][2]) & B[1][2]);
    A[4][3] = B[4][3] ^ ((~B[0][3]) & B[1][3]);
    A[4][4] = B[4][4] ^ ((~B[0][4]) & B[1][4]);

    A[0][0] = A[0][0] ^ RCi;

    return A;
  }

  uint constant blocksize = 136;

  function bswap(uint64 input) internal pure returns (uint64 v) {
    v = input;

    // swap bytes
    v = ((v & 0xFF00FF00FF00FF00) >> 8) |
        ((v & 0x00FF00FF00FF00FF) << 8);

    // swap 2-byte long pairs
    v = ((v & 0xFFFF0000FFFF0000) >> 16) |
        ((v & 0x0000FFFF0000FFFF) << 16);

    // swap 4-byte long pairs
    v = (v >> 32) | (v << 32);
  }

  function sha3(bytes input) public pure returns (bytes32) {
    uint newLen = ((input.length + blocksize) / blocksize) * blocksize;
    bytes memory padded = new bytes(newLen);
    for (uint i = 0; i < input.length; i++) {
      padded[i] = input[i];
    }
    padded[input.length] = padded[input.length] | 0x06;
    padded[newLen-1] = padded[newLen-1] | 0x80;

    uint nblocks = padded.length / blocksize;

    uint64[17][] memory P = new uint64[17][](nblocks);
    for (i = 0; i < nblocks; i++) {
      for (uint j = 0; j < 17; j++) {
        for (uint k = 0; k < 8; k++) {
          P[i][j] = P[i][j] | (uint64(uint8(padded[i * blocksize + j * 8 + k])) << (k * 8));
        }
      }
    }

    uint64[5][5] memory S;
    
    for (uint x = 0; x < nblocks; x++) {
      S[0][0] = S[0][0] ^ P[x][0];
      S[1][0] = S[1][0] ^ P[x][1];
      S[2][0] = S[2][0] ^ P[x][2];
      S[3][0] = S[3][0] ^ P[x][3];
      S[4][0] = S[4][0] ^ P[x][4];
      S[0][1] = S[0][1] ^ P[x][5];
      S[1][1] = S[1][1] ^ P[x][6];
      S[2][1] = S[2][1] ^ P[x][7];
      S[3][1] = S[3][1] ^ P[x][8];
      S[4][1] = S[4][1] ^ P[x][9];
      S[0][2] = S[0][2] ^ P[x][10];
      S[1][2] = S[1][2] ^ P[x][11];
      S[2][2] = S[2][2] ^ P[x][12];
      S[3][2] = S[3][2] ^ P[x][13];
      S[4][2] = S[4][2] ^ P[x][14];
      S[0][3] = S[0][3] ^ P[x][15];
      S[1][3] = S[1][3] ^ P[x][16];
      S = keccak_f(S);
    }

    uint256 Z;	
    Z = (Z << 64) | bswap(S[0][0]);
    Z = (Z << 64) | bswap(S[1][0]);
    Z = (Z << 64) | bswap(S[2][0]);
    Z = (Z << 64) | bswap(S[3][0]);
    return bytes32(Z);
  }
}
