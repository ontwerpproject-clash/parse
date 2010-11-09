-- Automatically generated VHDL
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use std.textio.all;


package types is

     subtype tfvec_index is integer range -1 to integer'high;
     
     subtype signed_8 is signed (7 downto 0);
     
     type Z2TZLz2cUZRsigned_8signed_8 is
          record A : signed_8;
                 B : signed_8;
          end record;
     
     function show (s : std_logic;
                    paren : boolean)
                   return string;
     
     function show (b : boolean;
                    paren : boolean)
                   return string;
     
     function show (sint : signed;
                    paren : boolean)
                   return string;
     
     function show (uint : unsigned;
                    paren : boolean)
                   return string;
     
     function show (tup : Z2TZLz2cUZRsigned_8signed_8;
                    paren : boolean)
                   return string;

end package types;


package body types is

     function show (s : std_logic;
                    paren : boolean)
                   return string is
     begin
          if s = '1' then
               return "High";
          else
               return "Low";
          end if;
     end;
     
     function show (b : boolean;
                    paren : boolean)
                   return string is
     begin
          if b then
               return "True";
          else
               return "False";
          end if;
     end;
     
     function show (sint : signed;
                    paren : boolean)
                   return string is
     begin
          return integer'image(to_integer(sint));
     end;
     
     function show (uint : unsigned;
                    paren : boolean)
                   return string is
     begin
          return integer'image(to_integer(uint));
     end;
     
     function show (tup : Z2TZLz2cUZRsigned_8signed_8;
                    paren : boolean)
                   return string is
     begin
          return '(' & (show(tup.A, false) & ',' & show(tup.B, false)) & ')';
     end;

end package body types;
