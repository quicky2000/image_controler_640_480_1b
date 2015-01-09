--
--    This file is part of image_controler_640_480_1b
--    Copyright (C) 2011  Julien Thevenon ( julien_thevenon at yahoo.fr )
--
--    This program is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    This program is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with this program.  If not, see <http://www.gnu.org/licenses/>
--
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
  
  s_write_enable <= (others => write_enable);
  s_data_in <= (others => data_in);

  r <= (others => data_out(0));
  g <= (others => data_out(0));
  b <= (others => data_out(0));
end Behavioral;

