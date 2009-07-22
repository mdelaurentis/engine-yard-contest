package com.mdelaurentis.engineyardcontest;

import java.util.BitSet;

public class Bits {
  // Returns a bitset containing the values in bytes.
  public static BitSet toBitSet(byte... bytes) {
    BitSet bits = new BitSet(160);
    for (int i = 0; i < bytes.length; i++) {
      int x = new Byte(bytes[i]).intValue();
      if (x < 0) {
        x += 256;
      }
      for (int j = 0; j < 8; j++) {
        int bit = 1 << j;
        if ((x & bit) > 0) {
          bits.set(i * 8 + (8 - (j + 1)));          
        }
      }
    }
    return bits;
  }  
  
  public static void main (String... args) {
    System.out.println(toBitSet(Integer.valueOf(args[0]).byteValue()));
  }

}