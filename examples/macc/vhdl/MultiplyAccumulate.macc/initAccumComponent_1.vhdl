-- Automatically generated VHDL
use work.types.all;
use work.all;
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use std.textio.all;


entity initAccumComponent_1 is
     port (reszc9Pzc9P2 : out signed_8;
           clock : in std_logic;
           resetn : in std_logic);
end entity initAccumComponent_1;


architecture structural of initAccumComponent_1 is
begin
     reszc9Pzc9P2 <= to_signed(0, 8);
end architecture structural;
