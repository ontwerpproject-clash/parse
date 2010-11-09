-- Automatically generated VHDL
use work.types.all;
use work.all;
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use std.textio.all;


entity maccComponent_0 is
     port (dszc8Q2 : in Z2TZLz2cUZRsigned_8signed_8;
           casevalzc9dzc9d3 : out signed_8;
           clock : in std_logic;
           resetn : in std_logic);
end entity maccComponent_0;


architecture structural of maccComponent_0 is
     signal uzc922 : signed_8;
     signal argzc9nzc9n3 : signed_8;
     signal acczc902 : signed_8;
     signal yzc8Y2 : signed_8;
     signal xzc8W2 : signed_8;
begin
     casevalzc9dzc9d3 <= uzc922;
     
     uzc922 <= acczc902 + argzc9nzc9n3;
     
     argzc9nzc9n3 <= resize(xzc8W2 * yzc8Y2, 8);
     
     yzc8Y2 <= dszc8Q2.B;
     
     xzc8W2 <= dszc8Q2.A;
     
     state : block
          signal initAccumval : signed_8;
     begin
          resetval_initAccumraup4 : entity initAccumComponent_1
                                         port map (reszc9Pzc9P2 => initAccumval,
                                                   clock => clock,
                                                   resetn => resetn);
          stateupdate : process (clock, resetn, initAccumval)
          begin
               if resetn = '0' then
                    acczc902 <= initAccumval;
               elsif rising_edge(clock) then
                    acczc902 <= uzc922;
               end if;
          end process stateupdate;
     end block state;
end architecture structural;
