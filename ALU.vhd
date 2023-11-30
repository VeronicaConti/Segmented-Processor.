library IEEE;
use IEEE.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity ALU is
end ALU;

architecture behavior of ALU is

component ALU

	Port (  a : in STD_LOGIC_VECTOR (31 downto 0);
    		b : in STD_LOGIC_VECTOR (31 downto 0);
            sel : in STD_LOGIC_vector(2 downto 0);
            o : out STD_LOGIC_VECTOR (31 downto 0);
            zero: out STD_LOGIC
            );
            
end component;
	
signal a : STD_LOGIC_VECTOR (31 downto 0);
signal b : STD_LOGIC_VECTOR (31 downto 0);
signal sel : STD_LOGIC_VECTOR (2 downto 0);
signal o : STD_LOGIC_VECTOR (31 downto 0);
signal zero: STD_LOGIC;

begin
end behavior;