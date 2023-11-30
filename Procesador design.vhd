-- Code your design here
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.STD_LOGIC_UNSIGNED.all;

entity processor is
port(
	Clk         : in  std_logic;
	Reset       : in  std_logic;
	-- Instruction memory
	I_Addr      : out std_logic_vector(31 downto 0);
	I_RdStb     : out std_logic;
	I_WrStb     : out std_logic;
	I_DataOut   : out std_logic_vector(31 downto 0);
	I_DataIn    : in  std_logic_vector(31 downto 0);
	-- Data memory
	D_Addr      : out std_logic_vector(31 downto 0);
	D_RdStb     : out std_logic;
	D_WrStb     : out std_logic;
	D_DataOut   : out std_logic_vector(31 downto 0);
	D_DataIn    : in  std_logic_vector(31 downto 0)

);
end processor;

architecture processor_arq of processor is 

    component Unidad_Control
    Port (
             ID_Instruct        : in std_logic_vector(5 downto 0); --de I_DataOut
             ID_RegWrite        : out std_logic;                      
             ID_MemToReg        : out std_logic;
             ID_BranchEquals    : out std_logic;    
             ID_BranchNonEquals : out std_logic;    
             ID_MemRead         : out std_logic;    
             ID_MemWrite        : out std_logic;    
             ID_RegDst          : out std_logic;  
             ID_AluOp           : out std_logic_vector(1 downto 0);    
             ID_AluSrc          : out std_logic
             );
             
     end component;        
    

	component BancoRegistros 
	port ( 	clk      : in std_logic;
    		reset    : in std_logic;
            wr       : in std_logic;
    		reg1_rd  : in std_logic_vector(4 downto 0); 
            reg2_rd  : in std_logic_vector(4 downto 0); 
            reg_wr   : in std_logic_vector(4 downto 0); 
            data_wr  : in std_logic_vector(31 downto 0);
            data1_rd : out std_logic_vector(31 downto 0);
            data2_rd : out std_logic_vector(31 downto 0)
            );
            
    end component;

    component ALU
    Port (  
    		a    : in std_logic_vector(31 downto 0);
    		b    : in std_logic_vector(31 downto 0);
            sel  : in std_logic_vector(2 downto 0);
            o    : out std_logic_vector(31 downto 0);
            zero : out std_logic
            );
            
	end component;
	
    signal clock: std_logic;
    signal rst: std_logic := '0';
       
    -- Señales de la etapa IF
    
	signal IF_PcIn    : std_logic_vector(31 downto 0); 	--pc imput
	signal IF_PCOut   : std_logic_vector(31 downto 0);	--pc output
	signal IF_Next    : std_logic_vector(31 downto 0);	-- pc+4
	signal IF_DataOut : std_logic_vector(31 downto 0);
	
    -- Señales de IFID
    
	signal IFID_Next : std_logic_vector(31 downto 0);
	signal IFID_Inst : std_logic_vector(31 downto 0);
    
    -- Señales de la etapa ID	
    
	signal ID_Instruction : std_logic_vector(31 downto 0);
	signal ID_SignExt     : std_logic_vector(31 downto 0);
	signal ID_DataReg1    : std_logic_vector(31 downto 0);
	signal ID_DataReg2    : std_logic_vector(31 downto 0);
	signal ID_Next        : std_logic_vector(31 downto 0);
	
	-- Señales de Unidad de Control 
    
	signal ID_RegWrite        : std_logic;
	signal ID_MemToReg        : std_logic;
	signal ID_BranchEquals    : std_logic;
    signal ID_BranchNonEquals : std_logic;
	signal ID_MemRead         : std_logic;
	signal ID_MemWrite        : std_logic;
	signal ID_RegDst          : std_logic;
	signal ID_AluOp           : std_logic_vector(1 downto 0);
	signal ID_AluSrc          : std_logic;
	
	-- Señales de seg ID/EX	
    
	signal IDEX_RegWrite        : std_logic;
	signal IDEX_MemToReg        : std_logic;
	signal IDEX_BranchEquals    : std_logic;
    signal IDEX_BranchNonEquals : std_logic;
	signal IDEX_MemRead         : std_logic;
	signal IDEX_MemWrite        : std_logic;
	signal IDEX_RegDst          : std_logic;
	signal IDEX_AluOp           : std_logic_vector(1 downto 0);
	signal IDEX_AluSrc          : std_logic;
	signal IDEX_PcNext          : std_logic_vector(31 downto 0); 
    signal IDEX_DataReg1        : std_logic_vector(31 downto 0);
    signal IDEX_DataReg2        : std_logic_vector(31 downto 0);
    signal IDEX_SignExt         : std_logic_vector(31 downto 0);
	signal IDEX_Rt              : std_logic_vector(4 downto 0);
	signal IDEX_Rd              : std_logic_vector(4 downto 0);

	--Señales de la etapa EX
    
	signal EXE_AluControl      : std_logic_vector(2 downto 0);
	signal EXE_SignExt         : std_logic_vector(31 downto 0);
	signal EXE_AluOp           : std_logic_vector(1 downto 0);
	signal EXE_RegDst          : std_logic;
	signal EXE_Rt              : std_logic_vector(4 downto 0);
	signal EXE_Rd              : std_logic_vector(4 downto 0);
	signal EXE_BranchAddress   : std_logic_vector(31 downto 0);
	signal EXE_Result          : std_logic_vector(31 downto 0);
	signal EXE_zero            : std_logic;
	signal EXE_AluMux          : std_logic_vector(31 downto 0);
	signal EXE_DataReg1        : std_logic_vector(31 downto 0);
	signal EXE_DataReg2        : std_logic_vector(31 downto 0);
	signal EXE_AluSrc          : std_logic;
	signal EXE_PcNext          : std_logic_vector(31 downto 0);
	signal EXE_WriteDest       : std_logic_vector(4 downto 0);
	signal EXE_BranchEquals    : std_logic;
	signal EXE_BranchNonEquals : std_logic;
	signal EXE_MemWrite        : std_logic;
    signal EXE_MemRead         : std_logic;
    signal EXE_MemToReg        : std_logic;
    signal EXE_RegWrite        : std_logic;

	
    --Señales de seg EX/MEM
    
	signal EXMEM_BranchAddress   : std_logic_vector(31 downto 0);
	signal EXMEM_Zero            : std_logic;
	signal EXMEM_BranchEquals    : std_logic;
	signal EXMEM_BranchNonEquals : std_logic;
	signal EXMEM_MemWrite        : std_logic;
	signal EXMEM_MemRead         : std_logic; 
	signal EXMEM_MemToReg        : std_logic;
	signal EXMEM_RegWrite        : std_logic;
	signal EXMEM_Result          : std_logic_vector(31 downto 0);
	signal EXMEM_WriteDest       : std_logic_vector(4 downto 0);
	signal EXMEM_DataReg2        : std_logic_vector(31 downto 0);


	--Señales de Memoria MEM
    
    signal MEM_WriteDest       : std_logic_vector(4 downto 0);
    signal MEM_BranchEquals    : std_logic;
    signal MEM_BranchNonEquals : std_logic;
    signal MEM_MemWrite        : std_logic;
    signal MEM_MemRead         : std_logic;
    signal MEM_MemToReg        : std_logic;
    signal MEM_RegWrite        : std_logic;
    signal MEM_Zero            : std_logic;
    signal MEM_Result          : std_logic_vector(31 downto 0);
    signal MEM_DataReg2        : std_logic_vector(31 downto 0);
	signal MEM_PcSrc           : std_logic; --selec de salto
	signal MEM_BranchAddress   : std_logic_vector(31 downto 0);--dir de salto
	signal MEM_DataOut         : std_logic_vector(31 downto 0);
	
	--Señales de seg MEM/WB
    
	signal MEMWB_RegWrite  : std_logic; 
    signal MEMWB_MemToReg  : std_logic;
    signal MEMWB_Result    : std_logic_vector (31 downto 0);
    signal MEMWB_WriteDest : std_logic_vector(4 downto 0);
	signal MEMWB_DataOut   : std_logic_vector(31 downto 0);

	--Señales de WriteBack WB
    
	signal WB_MemToReg    : std_logic;
	signal WB_DataOut     : std_logic_vector(31 downto 0);
	signal WB_Result      : std_logic_vector(31 downto 0);
	signal WB_RegWrite    : std_logic;
	signal WB_MuxWbResult : std_logic_vector(31 downto 0);
	signal WB_WriteDest   : std_logic_vector(4 downto 0);
    
  
begin 	

	clock <= clk;
    rst <= reset; 
    
------------------------ IF ---------------------------------------   
    
    -- Conexion al MIPS
    
	I_Addr     <= IF_PCOut;
	I_RdStb    <= '1'; 
	I_WrStb    <= '0';
	I_DataOut  <= x"00000000";
	IF_DataOut <= I_DataIn;
    
    IF_PcIn <= MEM_BranchAddress when
    	MEM_PcSrc  = '1' 
    else 
    	IF_Next;  --mux p/ saltos de inst
    
     pc_proceso: process(clock,rst)
     begin
     	if (reset = '1') then
        	IF_PCOut <= (others =>'0');
        elsif (rising_edge(clock)) then
        	IF_PCOut <= IF_PcIn;
        end if;
     end process;
     
     IF_Next <= IF_PCOut + x"00000004";
     	
	Reg_Seg_IFID: process(clock,rst)
    begin
    	
        if reset = '1' then
        	IFID_Inst <= (others => '0');
            IFID_Next <= (others => '0');
    	elsif (rising_edge(clock)) then
        	IFID_Inst <= IF_DataOut;
            IFID_Next <= IF_Next;
        end if;
    end process;
    
-----------------------FIN IF-------------------------------------------
------------------------ ID---------------------------------------------
    
  banco_reg: BancoRegistros 
	 Port Map ( 
                clk      => clock,
                reset    => rst,
                wr       => WB_RegWrite,
                reg1_rd  => ID_Instruction(25 downto 21),
                reg2_rd  => ID_Instruction(20 downto 16),
                reg_wr   => WB_WriteDest,
                data_wr  => WB_MuxWbResult,
                data1_rd => ID_DataReg1,
                data2_rd => ID_DataReg1
            	);
            
    	
 UC: Unidad_Control 
 	Port Map ( 
    		ID_Instruct        => ID_Instruction(31 downto 26),
          	ID_RegWrite        => ID_RegWrite,
            ID_MemToReg        => ID_MemToReg, 
            ID_BranchEquals    => ID_BranchEquals,
            ID_BranchNonEquals => ID_BranchNonEquals,
            ID_MemRead         => ID_MemRead,
            ID_MemWrite        => ID_MemWrite,
            ID_RegDst          => ID_RegDst,
            ID_AluOp           => ID_AluOp,
            ID_AluSrc          => ID_AluSrc	
    		);
            
          ID_Next <= IFID_Next;
	      ID_Instruction <= IFID_Inst; 
    
	--Signo Extendido
    
	     ID_SignExt <= x"0000" & ID_Instruction(15 downto 0) when                    						(ID_Instruction(15) = '0') 
         else  (x"FFFF" & ID_Instruction(15 downto 0));
   
	 Reg_Seg_ID_EX: process(clock,rst)
    	begin
          if (rst = '1') then
              IDEX_PcNext   <=(others => '0');
              IDEX_DataReg1 <=(others => '0');
              IDEX_DataReg2 <=(others => '0');
              IDEX_SignExt  <=(others => '0');
              IDEX_RegDst   <= '0';
              IDEX_AluSrc   <= '0';
              IDEX_AluOp        <= "00";
              IDEX_BranchEquals <= '0';
          	  IDEX_BranchNonEquals <= '0';
              IDEX_MemWrite <='0';
              IDEX_MemRead  <= '0';
              IDEX_MemToReg <= '0';
              IDEX_RegWrite <='0';
          elsif (rising_edge(clock)) then
              IDEX_PcNext   <= ID_Next;			
              IDEX_DataReg1 <= ID_DataReg1;		
              IDEX_DataReg2 <= ID_DataReg2;		
              IDEX_SignExt  <= ID_SignExt;		
              IDEX_RegDst   <= ID_RegDst;		
              IDEX_AluSrc   <= ID_AluSrc;		
              IDEX_AluOp    <= ID_AluOp;			
              IDEX_Rt <= ID_Instruction(20 downto 16); 
              IDEX_Rd <= ID_Instruction(15 downto 11); 
              IDEX_BranchEquals    <= ID_BranchEquals;
              IDEX_BranchNonEquals <= ID_BranchNonEquals;
              IDEX_MemWrite <= ID_MemWrite;
              IDEX_MemRead  <= ID_MemRead;
              IDEX_MemToReg <= ID_MemToReg;
              IDEX_RegWrite <= ID_RegWrite;
		end if;
	end process;
            
  ------ETAPA EX-------------------------------------------------------------------
    
  UnidadAritmeticaLogica: ALU 
  port map( a => EXE_DataReg1,
            b => EXE_AluMux,
            sel =>EXE_AluControl,
            o => EXE_Result,
            zero => EXE_zero
            );
            
    EXE_DataReg1 <= IDEX_DataReg1;
    EXE_Rt <= IDEX_Rt;
    EXE_Rd <= IDEX_Rd;
    EXE_SignExt <= IDEX_SignExt;
    EXE_AluOp <= IDEX_AluOp;
    EXE_DataReg2 <= IDEX_DataReg2;
    EXE_AluSrc <= IDEX_AluSrc;
    EXE_PcNext <= IDEX_PcNext;
    EXE_RegDst <= IDEX_RegDst;
    EXE_BranchEquals <= IDEX_BranchEquals;
    EXE_BranchNonEquals <= IDEX_BranchNonEquals;
    EXE_MemWrite <= IDEX_MemWrite;
    EXE_MemRead <= IDEX_MemRead;
    EXE_MemToReg <= IDEX_MemToReg;
    EXE_RegWrite <= IDEX_RegWrite;         
            
    EXE_BranchAddress <= EXE_PcNext + (EXE_SignExt(29 downto 0) & "00");

    EXE_AluMux <= EXE_DataReg2 when 
    	(EXE_AluSrc = '0') 
    else 
    	EXE_SignExt;

    EXE_WriteDest <= EXE_Rt when
    	(EXE_RegDst = '0')
    else 
    	 EXE_Rd;
    
    AluControl: process (EXE_SignExt(5 downto 0), EXE_AluOp)
    begin
      case(EXE_AluOp) is
       	when "11" => --Tipo R
            	case (EXE_SignExt(5 downto 0)) is 
                	when "100000"=>  	--ADD                  
                	   	EXE_AluControl <= "010";   
                    when"100010" => 	--SUB 
                        EXE_AluControl <= "110";
                    when "100100" =>	 -- AND
                        EXE_AluControl <= "000";
                    when "100101" =>	 -- OR
                        EXE_AluControl <= "010";
                    when "101010" =>	 -- SLT
                        EXE_AluControl <= "100";
				    when others => 
					    EXE_AluControl <= "000";
            	 end case;
                    
        when "01" =>  --BEQ 110
          EXE_AluControl <= "110";
        when "00" =>  -- MEM 010
          EXE_AluControl <= "010";
        when others =>  
          EXE_AluControl <= "000"; 
                end case;   
	end process;

 Reg_Seg_EX_MEM: process(clock,rst)
    begin
    	
        if (rst = '1') then
            EXMEM_BranchAddress <= (others => '0');
            EXMEM_WriteDest     <= (others => '0');
            EXMEM_BranchEquals  <= '0';
            EXMEM_BranchNonEquals <= '0';
            EXMEM_MemWrite <= '0';
            EXMEM_MemRead  <= '0'; 
            EXMEM_MemToReg <= '0';
            EXMEM_RegWrite <= '0';
            EXMEM_Zero   <= '0';
            EXMEM_Result <= (others => '0');
            EXMEM_DataReg2 <= (others => '0');
		elsif (rising_edge(clock)) then
		    EXMEM_BranchAddress <= EXE_BranchAddress;
            EXMEM_WriteDest <= EXE_WriteDest;
            EXMEM_BranchEquals <= EXE_BranchEquals;
            EXMEM_BranchNonEquals <= EXE_BranchNonEquals;
            EXMEM_MemWrite <= EXE_MemWrite;
            EXMEM_MemRead <= EXE_MemRead; 
            EXMEM_MemToReg <= EXE_MemToReg;
            EXMEM_RegWrite <= EXE_RegWrite;
            EXMEM_Zero <= EXE_Zero;
            EXMEM_Result <=EXE_Result;
            EXMEM_DataReg2 <= EXE_DataReg2;
       end if;
    end process; 
    
-------------FIN EX/MEM------------------------------------------

----------------MEM--------------------------------------------------
   
    MEM_WriteDest <= EXMEM_WriteDest;
    MEM_BranchEquals <= EXMEM_BranchEquals;
    MEM_BranchNonEquals <= EXMEM_BranchNonEquals;
    MEM_MemWrite <= EXMEM_MemWrite;
    MEM_MemRead <= EXMEM_MemRead; 
    MEM_MemToReg <= EXMEM_MemToReg;
    MEM_RegWrite <= EXMEM_RegWrite;
    MEM_Zero <= EXMEM_Zero;
    MEM_Result <= EXMEM_Result;
    MEM_DataReg2 <= EXMEM_DataReg2;
    MEM_BranchAddress <= EXMEM_BranchAddress;


    D_Addr <= MEM_Result;
    D_RdStb <= MEM_MemRead;
    D_WrStb <= MEM_MemWrite;
    D_DataOut <= MEM_DataReg2;
    MEM_DataOut <= D_DataIn;

    MEM_PcSrc <= (MEM_BranchEquals and MEM_Zero) or (MEM_BranchNonEquals and not MEM_Zero);

    Reg_Seg_MEM_WB:process (clock,rst)
	  begin
		if (rst = '1') then
		    MEMWB_RegWrite  <= '0';
		    MEMWB_MemToReg  <= '0';
		    MEMWB_DataOut   <= (others => '0');
		    MEMWB_Result    <= (others => '0');
		    MEMWB_WriteDest <= (others => '0');
		    
		elsif (rising_edge(clock)) then
		    MEMWB_RegWrite  <= MEM_RegWrite;
		    MEMWB_MemToReg  <= MEM_MemToReg;
		    MEMWB_DataOut   <= MEM_DataOut;
		    MEMWB_Result    <= MEM_Result;
		    MEMWB_WriteDest <= MEM_WriteDest;
         end if;
     end process;
     
----------------FIN MEM/WB--------------------------------------------

-------------------WB--------------------------------------------------
WB_RegWrite  <= MEMWB_RegWrite;
WB_MemToReg  <= MEMWB_MemToReg;
WB_DataOut   <= MEMWB_DataOut;
WB_Result    <= MEMWB_Result;
WB_WriteDest <= MEMWB_WriteDest;

WB_MuxWbResult <= WB_DataOut when 
	(WB_MemToReg = '0') 
 else
 	WB_Result;

------------------FIN WB------------------------------------------   
          
end processor_arq;