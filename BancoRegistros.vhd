library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.NUMERIC_STD.all;

entity BancoRegistros is
	port ( 	clk: in STD_LOGIC;
    		reset: in STD_LOGIC;
            wr: in STD_LOGIC;
    		reg1_rd: in STD_LOGIC_VECTOR (4 downto 0); 
            reg2_rd: in STD_LOGIC_VECTOR (4 downto 0); 
            reg_wr: in STD_LOGIC_VECTOR (4 downto 0); 
            data_wr: in STD_LOGIC_VECTOR (31 downto 0);
            data1_rd :out STD_LOGIC_VECTOR (31 downto 0);
            data2_rd: out STD_LOGIC_VECTOR (31 downto 0)
            );
    end BancoRegistros;
    
architecture behavioral of BancoRegistros is
TYPE T_REG is ARRAY(0 to 31) of STD_LOGIC_VECTOR(31 downto 0);
signal reg : T_REG;

begin
	
    reset_escritura:process(clk,reset)
    begin
    	if (reset = '1') then
        	reg <= (others => x"00000000");
        elsif (Falling_Edge(clk)) then
        		if (wr = '1') then
        		     reg(to_integer(unsigned(reg_wr))) <= data_wr;
                end if;     
        end if;
    end process;
    
--lectura: process(reg1_rd, reg2_rd, reg)
  --  begin
    
    	data1_rd <= reg(to_integer(unsigned(reg1_rd)))when(reg1_rd/="00000") else x"00000000";
        data2_rd <= reg(to_integer(unsigned(reg2_rd)))when(reg2_rd/="00000") else x"00000000";     
        
  --  end process;
end;