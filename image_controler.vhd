----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    21:59:41 01/18/2011 
-- Design Name: 
-- Module Name:    image_controler - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity image_controler is
  Port ( clk : in  STD_LOGIC;
         rst : in  STD_LOGIC;
         r : out  STD_LOGIC_VECTOR (5 downto 0);
         g : out  STD_LOGIC_VECTOR (5 downto 0);
         b : out  STD_LOGIC_VECTOR (5 downto 0);
         x : in  STD_LOGIC_VECTOR (9 downto 0);
         y : in  STD_LOGIC_VECTOR (8 downto 0);
         enable_in : in std_logic;
         hsync_in : in std_logic;
         vsync_in : in std_logic;
         write_enable : in std_logic;
         write_addr : in std_logic_vector(18 downto 0);
         data_in : in std_logic;
         enable_out : out std_logic;
         hsync_out : out std_logic;
         vsync_out : out std_logic);
end image_controler;

architecture Behavioral of image_controler is
------------- Begin Cut here for COMPONENT Declaration ------ COMP_TAG
  component ram
    port (
      clka: IN std_logic;
      wea: IN std_logic_VECTOR(0 downto 0);
      addra: IN std_logic_VECTOR(18 downto 0);
      dina: IN std_logic_VECTOR(0 downto 0);
      clkb: IN std_logic;
      addrb: IN std_logic_VECTOR(18 downto 0);
      doutb: OUT std_logic_VECTOR(0 downto 0));
  end component;

-- Synplicity black box declaration
  attribute syn_black_box : boolean;
  attribute syn_black_box of ram: component is true;

-- COMP_TAG_END ------ End COMPONENT Declaration ------------

-- Signals needed to write in memory
  signal s_write_enable : std_logic_vector(0 downto 0);
--TEST signal write_addr : std_logic_vector(18 downto 0);
  signal s_data_in : std_logic_vector(0 downto 0);

-- Signals needed to read from memory
  signal read_addr : std_logic_vector(18 downto 0) := (others => '0');
  signal data_out : std_logic_vector(0 downto 0);

  constant controler_delay : natural := 1;
begin
--	process(clk,rst)
--	begin
--		if rst = '1' then
--			r <= (others => '0');
--			g <= (others => '0');
--			b <= (others => '0');
--		elsif rising_edge(clk) then
--			if unsigned(x) = 1 or unsigned(x) = 640 or unsigned(y) = 1 or unsigned(y) =480 then
--				r <= (others => '1');
--				g <= (others => '0');
--				b <= (others => '0');
--			else
--				r <= (others => '0');
--				g <= (others => '1');
--				b <= (others => '0');
--			end if;
--		end if;
--	end process;

--	process(x,y)
--	begin
--		if unsigned(x) = 0 or unsigned(x) = 639 or unsigned(y) = 0 or unsigned(y) =479 then
--			r <= (others => '1');
--			g <= (others => '0');
--			b <= (others => '0');
--		else
--			r <= (others => '0');
--			g <= (others => '1');
--			b <= (others => '0');
--		end if;
--	end process;

------------- Begin Cut here for INSTANTIATION Template ----- INST_TAG
  your_instance_name : ram
    port map (
      clka => clk,
      wea => s_write_enable,
      addra => write_addr,
      dina => s_data_in,
      clkb => clk,
      addrb => read_addr,
      doutb => data_out);
-- INST_TAG_END ------ End INSTANTIATION Template ------------

  -- Block to introduce delays need to have control signals
  -- synchronous with colour signals
  hsync_delayer : entity work.bit_delay
    generic map (
      size => controler_delay)
    port map (
      clk => clk,
      rst => rst,
      input => hsync_in,
      output => hsync_out);
  
  vsync_delayer : entity work.bit_delay 
    generic map (
      size => controler_delay)
    port map (
      clk => clk,
      rst => rst,
      input => vsync_in,
      output => vsync_out);
  
  enable_delayer : entity work.bit_delay 
    generic map (
      size => controler_delay)
    port map (
      clk => clk,
      rst => rst,
      input => enable_in,
      output => enable_out);
  
  -- Process controling adress of read port
  process(clk,rst)
  begin
    if rst = '1' then
      read_addr <= (others => '0');
    elsif rising_edge(clk) and enable_in = '1' then
      if unsigned(read_addr) /= 307199 then
        read_addr <= std_logic_vector(unsigned(read_addr) + 1);
      else 
        read_addr <= (others => '0');
      end if;
    end if;
  end process;
  
  -- Process writing in memory
--TEST 	process(clk,rst)
--TEST 		constant x_max : positive := 639;
--TEST 		constant y_max : positive := 479;
--TEST 		variable x_write : natural range 0 to x_max := 0;
--TEST 		variable y_write : natural range 0 to y_max := 0;
--TEST 		variable addr : natural range 0 to 307199 := 0;
--TEST 	begin
--TEST 		if rst = '1' then
--TEST 			write_enable <= (others => '0');
--TEST 			write_addr <= (others => '0');
--TEST 			data_in <= (others => '0');		
--TEST 		elsif rising_edge(clk) then
--TEST 			if std_logic_vector(to_unsigned(x_write,4)) = "0000" or std_logic_vector(to_unsigned(y_write,4)) = "0000" then
--TEST 				write_enable <= (others => '1');
--TEST 				write_addr <= std_logic_vector(to_unsigned(addr,19));
--TEST 				data_in <= (others => '1');
--TEST 			else
--TEST 				write_enable <= (others => '0');
--TEST 				write_addr <= (others => '0');
--TEST 				data_in <= (others => '0');
--TEST 			end if;
--TEST 
--TEST 			-- Address management 
--TEST 			if addr /= 307199 then
--TEST 				addr := addr + 1;
--TEST 			else
--TEST 				addr := 0;
--TEST 			end if; -- addr max
--TEST 			-- Coordinate management
--TEST 			if x_write /= x_max then
--TEST 				x_write := x_write + 1;
--TEST 			else --xmax
--TEST 				x_write := 0;
--TEST 				if y_write /= y_max then
--TEST 					y_write := y_write + 1;
--TEST 				else
--TEST 					y_write := 0;
--TEST 				end if;	--ymax
--TEST 			end if; -- xmax
--TEST 		end if;-- clock rising edge
--TEST 	end process;
  
  s_write_enable <= (others => write_enable);
  s_data_in <= (others => data_in);

  r <= (others => data_out(0));
  g <= (others => data_out(0));
  b <= (others => data_out(0));
end Behavioral;

