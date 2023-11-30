--unity control
library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_UNSIGNED.all;

entity Unidad_Control is
port(
     ID_Instruct   : in std_logic_vector(5 downto 0); --viene de I_DataOut
                                                         --son sus 6bits mas significativos
     ID_RegWrite : out std_logic;                          
     ID_MemToReg : out std_logic;
     ID_BranchEquals : out std_logic;    
     ID_BranchNonEquals : out std_logic;    
     ID_MemRead  : out std_logic;    
     ID_MemWrite : out std_logic;    
     ID_RegDst   : out std_logic;  
     ID_AluOp    : out std_logic_vector(1 downto 0);    
     ID_AluSrc   : out std_logic
);
end Unidad_Control;

architecture control_arq of Unidad_Control is

     begin
          Control:process(ID_Instruct(5 downto 0))
          begin
               case ID_Instruct(5 downto 0) is
                    when "000000" => -- R-Type
                         ID_RegWrite    <= '1';
                         ID_MemToReg  <= '0';
                         ID_BranchEquals  <= '0';
                         ID_BranchNonEquals  <= '0';
                         ID_MemRead    <= '0';
                         ID_MemWrite   <= '0';
                         ID_RegDst   <= '1';
                         ID_AluOp    <= "10";
                         ID_AluSrc   <= '0';
                    when "100011" => --LOAD
                         ID_RegWrite    <= '1';
                         ID_MemToReg  <= '1';
                         ID_BranchEquals  <= '0';
                         ID_BranchNonEquals  <= '0';
                         ID_MemRead    <= '1';
                         ID_MemWrite   <= '0';
                         ID_RegDst   <= '0';
                         ID_AluOp    <= "00";
                         ID_AluSrc   <= '1';
                    when "101011" => --STORE
                         ID_RegWrite    <= '0';
                         ID_MemToReg  <= '0';
                         ID_BranchEquals  <= '0';
                         ID_BranchNonEquals  <= '0';
                         ID_MemRead    <= '0';
                         ID_MemWrite   <= '1';
                         ID_RegDst   <= '0';
                         ID_AluOp    <= "00";
                         ID_AluSrc   <= '1';
                    when "000100" => --BEQ        
                         ID_RegWrite    <= '0';
                         ID_MemToReg  <= '0';
                         ID_BranchEquals  <= '1';
                         ID_BranchNonEquals  <= '0';
                         ID_MemRead    <= '0';
                         ID_MemWrite   <= '0';
                         ID_RegDst   <= '0';
                         ID_AluOp    <= "01";
                         ID_AluSrc   <= '0';
                    when "000101" => --BNE        
                         ID_RegWrite    <= '0';
                         ID_MemToReg  <= '0';
                         ID_BranchEquals  <= '0';
                         ID_BranchNonEquals  <= '1';
                         ID_MemRead    <= '0';
                         ID_MemWrite   <= '0';
                         ID_RegDst   <= '0';
                         ID_AluOp    <= "01";
                         ID_AluSrc   <= '0';
                    when "001111" => --LUI      
                         ID_RegWrite    <= '1';
                         ID_MemToReg  <= '1';
                         ID_BranchEquals  <= '0';
                         ID_BranchNonEquals  <= '0';
                         ID_MemRead    <= '0';
                         ID_MemWrite   <= '0';
                         ID_RegDst   <= '0';
                         ID_AluOp    <= "00";
                         ID_AluSrc   <= '1';
                    when others => --OTHERS
                         ID_RegWrite    <= '0';
                         ID_MemToReg  <= '0';
                         ID_BranchEquals  <= '0';
                         ID_BranchNonEquals  <= '0';
                         ID_MemRead    <= '0';
                         ID_MemWrite   <= '0';
                         ID_RegDst   <= '0';
                         ID_AluOp    <= "00";
                         ID_AluSrc   <= '0';
                         
               end case;
          end process;
end control_arq;