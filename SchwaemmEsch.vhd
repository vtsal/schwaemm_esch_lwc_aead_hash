----------------------------------------------------------------------------------
-- Code based on NIST LWC Schwaemm256128
-- 2/6/2020
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity regGen is
generic( width: integer );
port(
	d: in std_logic_vector(width - 1 downto 0);
	e, clk: in std_logic;
	q: out std_logic_vector(width - 1 downto 0)
	);
end regGen;

architecture behavioral of regGen is
begin

    process(clk) begin
        if (rising_edge(clk)) then
            if (e = '1') then
                q <= d;
            end if;
        end if;
    end process;

end behavioral;

library ieee;
use ieee.std_logic_1164.all;

entity regOne is
port(
	d: in std_logic;
	e, clk: in std_logic;
	q: out std_logic
	);
end regOne;

architecture behavioral of regOne is
begin

    process(clk) begin
        if (rising_edge(clk)) then
            if (e = '1') then
                q <= d;
            end if;
        end if;
    end process;

end behavioral;

library ieee;
use ieee.std_logic_1164.all;

entity regNum is
port(
	d: in integer;
	e, clk: in std_logic;
	q: out integer
	);
end regNum;

architecture behavioral of regNum is
begin

    process(clk) begin
        if (rising_edge(clk)) then
            if (e = '1') then
                q <= d;
            end if;
        end if;
    end process;

end behavioral;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use work.utility_functions.ALL;

entity arx_round is
Port ( 
    round_constant : in std_logic_vector(31 downto 0);
    round : in integer;
    x_round_in, y_round_in : in std_logic_vector(31 downto 0);
    x_round_out, y_round_out : out std_logic_vector(31 downto 0)
    );
end arx_round;

architecture behavioral of arx_round is
    signal sum : std_logic_vector(31 downto 0);
    signal y_rotated, sum_rotated : std_logic_vector(31 downto 0);
begin
    
    y_rotated <= rot_y_word(y_round_in, round); 
    sum <= x_round_in + y_rotated;

    sum_rotated <= rot_x_word(sum, round);
    y_round_out <= y_round_in xor sum_rotated;
    x_round_out <= sum xor round_constant;

end behavioral;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use work.utility_functions.ALL;

entity linear_layer is
    Port (
        state_in : in std_logic_vector(383 downto 0);
        state_out : out std_logic_vector(383 downto 0)
    );
end linear_layer;

architecture structural of linear_layer is

    -- Function to compute x tmp or y tmp
    function ell ( input_word : in std_logic_vector(31 downto 0))
    return std_logic_vector is variable tmp : std_logic_vector(31 downto 0);
    begin 
        tmp := (input_word(15 downto 0) & input_word(31 downto 16)) xor (x"0000" & input_word(15 downto 0));
        return tmp;
    end ell;
    
    signal xor_result_x, xor_result_y : std_logic_vector(31 downto 0);
    signal x_tmp, y_tmp : std_logic_vector(31 downto 0);

begin

    xor_result_y <= state_in(351 downto 320) xor state_in(287 downto 256) xor state_in(223 downto 192);
    xor_result_x <= state_in(383 downto 352) xor state_in(319 downto 288) xor state_in(255 downto 224);
    y_tmp <= ell(xor_result_y);
    x_tmp <= ell(xor_result_x);

    -- Update "x" words of state
    state_out(383 downto 352) <= state_in(319 downto 288) xor state_in(127 downto 96) xor y_tmp;
    state_out(319 downto 288) <= state_in(255 downto 224) xor state_in(63 downto 32) xor y_tmp;
    state_out(255 downto 224) <= state_in(383 downto 352) xor state_in(191 downto 160) xor y_tmp;
    state_out(191 downto 160) <= state_in(383 downto 352);
    state_out(127 downto 96) <= state_in(319 downto 288);
    state_out(63 downto 32) <= state_in(255 downto 224);

    -- Update "y" words of state
    state_out(351 downto 320) <= state_in(287 downto 256) xor state_in(95 downto 64) xor x_tmp;
    state_out(287 downto 256) <= state_in(223 downto 192) xor state_in(31 downto 0) xor x_tmp;
    state_out(223 downto 192) <= state_in(351 downto 320) xor state_in(159 downto 128) xor x_tmp;
    state_out(159 downto 128) <= state_in(351 downto 320);
    state_out(95 downto 64) <= state_in(287 downto 256);
    state_out(31 downto 0) <= state_in(223 downto 192);

end structural;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use work.utility_functions.ALL;

entity feistel_function is
    Port (
        state_in : in std_logic_vector(383 downto 0);
        state_out : out std_logic_vector(383 downto 0)
    );
end feistel_function;

architecture structural of feistel_function is

    -- Function to compute x tmp or y tmp
    function ell ( input_word : in std_logic_vector(31 downto 0))
    return std_logic_vector is variable tmp : std_logic_vector(31 downto 0);
    begin 
        tmp := (input_word(15 downto 0) & input_word(31 downto 16)) xor (x"0000" & input_word(15 downto 0));
        return tmp;
    end ell;
    
    signal xor_result_x, xor_result_y : std_logic_vector(31 downto 0);
    signal x_tmp, y_tmp : std_logic_vector(31 downto 0);

begin

    xor_result_y <= state_in(351 downto 320) xor state_in(287 downto 256) xor state_in(223 downto 192);
    xor_result_x <= state_in(383 downto 352) xor state_in(319 downto 288) xor state_in(255 downto 224);
    y_tmp <= ell(xor_result_y);
    x_tmp <= ell(xor_result_x);
    
    -- Update "x" words of state
    state_out(383 downto 352) <= state_in(383 downto 352) xor y_tmp;
    state_out(319 downto 288) <= state_in(319 downto 288) xor y_tmp;
    state_out(255 downto 224) <= state_in(255 downto 224) xor y_tmp;
    state_out(191 downto 160) <= state_in(191 downto 160);
    state_out(127 downto 96) <= state_in(127 downto 96);
    state_out(63 downto 32) <= state_in(63 downto 32);

    -- Update "y" words of state
    state_out(351 downto 320) <= state_in(351 downto 320) xor x_tmp;
    state_out(287 downto 256) <= state_in(287 downto 256) xor x_tmp;
    state_out(223 downto 192) <= state_in(223 downto 192) xor x_tmp;
    state_out(159 downto 128) <= state_in(159 downto 128);
    state_out(95 downto 64) <= state_in(95 downto 64);
    state_out(31 downto 0) <= state_in(31 downto 0);
    
    

end structural;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.utility_functions.ALL;

entity sparkle_permutation_fsm is
    Port ( 
        clk : in std_logic;
        rst : in std_logic;
        perm_start : in std_logic;
        num_steps : in integer;
        state_in : in std_logic_vector(383 downto 0);
        state_out : out std_logic_vector(383 downto 0);
        perm_complete : out std_logic
        );
end sparkle_permutation_fsm;

architecture behavioral of sparkle_permutation_fsm is

    type word_constants is array(0 to 7) of std_logic_vector (31 downto 0);
       constant round_constant_array : word_constants := (x"B7E15162", x"BF715880", x"38B4DA56", x"324E7738",
                                                          x"BB1185EB", x"4F7C7B57", x"CFBFA1C8", x"C2B3293D");
                               
    type perm_state is (IDLE, RUN);
        signal current_state : perm_state;
        signal next_state : perm_state;
        
	signal state_in_mod : std_logic_vector(383 downto 0);
	signal arx_state_in, arx_state_out : std_logic_vector(383 downto 0);
	signal arx_step_in, perm_reg_in, perm_reg_out : std_logic_vector(383 downto 0);
--	signal x_rot_signal, y_rot_signal : integer := 0;
	
	signal linear_state_in, linear_state_out : std_logic_vector(383 downto 0);
	
	signal round_counter : integer := 0;
	signal step_counter : integer := 0;
	signal arx_cntr_en, arx_cntr_init, perm_cntr_en, perm_cntr_init : std_logic;

begin

    -- MUX to select ARX STEP input (state input vs. linear layer output)
    with perm_start select
    arx_step_in <= linear_state_out when '0', 
                   state_in when '1',
                   state_in when others;
                   
    
    -- MUX to select ARX ROUND input (step input vs. arx output)
    with perm_cntr_en select
    perm_reg_in <= arx_state_out when '0',
                    arx_step_in when '1', 
                    arx_step_in when others;
                 
    -- Permutation state register
    state_reg: entity work.regGen(behavioral)
    generic map (width => 384)
    port map(
       d => perm_reg_in,
	   e => arx_cntr_en,
	   clk => clk,
	   q => perm_reg_out
    );
    
    -- Update words Y0 and Y1 at the start of each step (rc = 0)
    arx_state_in(383 downto 352) <= perm_reg_out(383 downto 352);
    arx_state_in(351 downto 320) <= perm_reg_out(351 downto 320) xor round_constant_array(step_counter mod 8) when (round_counter = 0) else perm_reg_out(351 downto 320);
    arx_state_in(319 downto 288) <= perm_reg_out(319 downto 288);
    arx_state_in(287 downto 256) <= perm_reg_out(287 downto 256) xor std_logic_vector(to_unsigned(step_counter, 32)) when (round_counter = 0) else perm_reg_out(287 downto 256);
    arx_state_in(255 downto 0) <= perm_reg_out(255 downto 0);

    -- Map linear layer input
    linear_state_in <= arx_state_out;

    perm_done_process: process(clk) begin
    if(rising_edge(clk)) then
        perm_complete <= '0';
        if (step_counter = num_steps -1) and (round_counter = 3) then
            perm_complete <= '1';
            state_out <= linear_state_out;
        end if;
    end if;
    end process;    
    
    arx_round_unit_0: entity work.arx_round(behavioral)
	port map(
	    round => round_counter,
	    round_constant => round_constant_array(0),
        x_round_in => arx_state_in(383 downto 352),
        y_round_in => arx_state_in(351 downto 320),
        x_round_out => arx_state_out(383 downto 352),
        y_round_out => arx_state_out(351 downto 320)
		);
		
    arx_round_unit_1: entity work.arx_round(behavioral)
	port map(
	    round => round_counter,
	    round_constant => round_constant_array(1),
        x_round_in => arx_state_in(319 downto 288),
        y_round_in => arx_state_in(287 downto 256),
        x_round_out => arx_state_out(319 downto 288),
        y_round_out => arx_state_out(287 downto 256)
		);
		
    arx_round_unit_2: entity work.arx_round(behavioral)
	port map(
	    round => round_counter,
	    round_constant => round_constant_array(2),
        x_round_in => arx_state_in(255 downto 224),
        y_round_in => arx_state_in(223 downto 192),
        x_round_out => arx_state_out(255 downto 224),
        y_round_out => arx_state_out(223 downto 192)
		);
		
    arx_round_unit_3: entity work.arx_round(behavioral)
	port map(
	    round => round_counter,
	    round_constant => round_constant_array(3),
        x_round_in => arx_state_in(191 downto 160),
        y_round_in => arx_state_in(159 downto 128),
        x_round_out => arx_state_out(191 downto 160),
        y_round_out => arx_state_out(159 downto 128)
		);
		
    arx_round_unit_4: entity work.arx_round(behavioral)
	port map(
	    round => round_counter,
	    round_constant => round_constant_array(4),
        x_round_in => arx_state_in(127 downto 96),
        y_round_in => arx_state_in(95 downto 64),
        x_round_out => arx_state_out(127 downto 96),
        y_round_out => arx_state_out(95 downto 64)
		);
		
    arx_round_unit_5: entity work.arx_round(behavioral)
	port map(
	    round => round_counter,
	    round_constant => round_constant_array(5),
        x_round_in => arx_state_in(63 downto 32),
        y_round_in => arx_state_in(31 downto 0),
        x_round_out => arx_state_out(63 downto 32),
        y_round_out => arx_state_out(31 downto 0)
		);

    linear_layer_unit: entity work.linear_layer(structural)
	port map(
	    state_in => linear_state_in,
        state_out => linear_state_out
		);

counter_process: process(clk) begin
	if (rising_edge(clk)) then
		if (arx_cntr_en = '1') then
			if (arx_cntr_init = '1') then
				round_counter <= 0;
			else
				round_counter <= round_counter + 1;
		    end if;
		end if;
		if (perm_cntr_en = '1') then
			if (perm_cntr_init = '1') then
				step_counter <= 0;
			else
				step_counter <= step_counter + 1;
		    end if;
		end if;
	end if;
end process;
	
sync_process: process(clk) begin
if (rising_edge(clk)) then
	if (rst = '1') then
	   current_state <= IDLE;
	else
	   current_state <= next_state;
	end if;
end if;
end process;

public_process: process(current_state, perm_start, round_counter, step_counter, num_steps)
begin
 
-- Defaults
arx_cntr_init <= '0';
arx_cntr_en <= '0';
perm_cntr_init <= '0';
perm_cntr_en <= '0';

case current_state is
		 		 
	when IDLE => 
        next_state <= IDLE; 
		if (perm_start = '1') then
		
            -- Start counters and reset to 0
            arx_cntr_en <= '1';
            arx_cntr_init <= '1';
            perm_cntr_init <= '1';
            perm_cntr_en <= '1';
            
			next_state <= RUN;
		end if;
	    
    when RUN => 

        next_state <= RUN;
        arx_cntr_en <= '1';                             -- Enable round counter

        if (round_counter = 3) then
            perm_cntr_en <= '1';                        -- Enable the step counter
            if (step_counter = (num_steps - 1)) then
                perm_cntr_init <= '1';                  -- Reset the permutation step counter
				next_state <= IDLE;
            end if;
            arx_cntr_init <= '1';                       -- Reset the arx round counter
		end if;

    when others =>
		next_state <= IDLE;
			  
end case; 

end process;
end behavioral;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use work.utility_functions.ALL;

entity rate_whitening is
    Port (
        state_in : in std_logic_vector(383 downto 0);
        state_out : out std_logic_vector(383 downto 0)
    );
end rate_whitening;

architecture structural of rate_whitening is
begin

    -- Update rate portion of state
    state_out(383 downto 256) <= state_in(383 downto 256) xor state_in(127 downto 0);
    state_out(255 downto 128) <= state_in(255 downto 128) xor state_in(127 downto 0);
    
    -- Capacity portion of state not modified
    state_out(127 downto 0) <= state_in(127 downto 0);

end structural;


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.utility_functions.ALL;

entity inject_constant is
    Port (
        state_in : in std_logic_vector(383 downto 0);
        constant_value : in std_logic_vector(31 downto 0);
        state_out : out std_logic_vector(383 downto 0)
    );
end inject_constant;

architecture structural of inject_constant is
begin

    -- Map state in to state out
    state_out(383 downto 32) <= state_in(383 downto 32);
    
    -- Update last word of state with constant
    state_out(31 downto 0) <= state_in(31 downto 0) xor constant_value;

end structural;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use work.utility_functions.ALL;

entity feistel_swap is
    Port (
        state_in : in std_logic_vector(383 downto 0);
        state_out : out std_logic_vector(383 downto 0)
    );
end feistel_swap;

architecture structural of feistel_swap is
begin

    -- Update rate part of state
    state_out(383 downto 256) <= state_in(255 downto 128);
    state_out(255 downto 128) <= state_in(255 downto 128) xor state_in(383 downto 256);
    
    -- Capacity not affected
    state_out(127 downto 0) <= state_in(127 downto 0);
    
end structural;


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use work.utility_functions.ALL;

entity rho is
    Port (
        state_in : in std_logic_vector(383 downto 0);
        input_rate : in std_logic_vector(255 downto 0);
        state_out : out std_logic_vector(383 downto 0)
    );
end rho;

architecture structural of rho is
begin

    -- Update rate part of state
    state_out(383 downto 128) <= state_in(383 downto 128) xor input_rate(255 downto 0);
    
    -- Capacity not affected
    state_out(127 downto 0) <= state_in(127 downto 0);
    
end structural;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use work.utility_functions.ALL;

entity inv_rho is
    Port (
        state_in_pre_feistel : in std_logic_vector(255 downto 0);
        state_in_post_feistel : in std_logic_vector(383 downto 0);
        input_rate : in std_logic_vector(255 downto 0);
        state_out : out std_logic_vector(383 downto 0)
    );
end inv_rho;

architecture structural of inv_rho is
begin

    -- Update rate part of state
    state_out(383 downto 128) <= state_in_post_feistel(383 downto 128) xor (state_in_pre_feistel(255 downto 0) xor input_rate(255 downto 0));
    
    -- Capacity not affected
    state_out(127 downto 0) <= state_in_post_feistel(127 downto 0);
    
end structural;

