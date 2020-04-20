----------------------------------------------------------------------------------
-- Code based on NIST LWC Schwaemm256128
-- 3/18/2020
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use work.utility_functions.ALL;
use work.NIST_LWAPI_pkg.ALL;

entity controller is
    Port (
        clk : in std_logic;
        rst : in std_logic;
    
        key : in std_logic_vector(31 downto 0);
        key_valid : in std_logic;
        key_ready : out std_logic;
        
        bdi : in std_logic_vector(31 downto 0);
        bdi_valid : in std_logic;
        bdi_ready : out std_logic;
        bdi_pad_loc : in std_logic_vector(3 downto 0);
        bdi_valid_bytes : in std_logic_vector(3 downto 0);
        
        bdi_size : in std_logic_vector(2 downto 0);
        bdi_eot : in std_logic;
        bdi_eoi : in std_logic;
        bdi_type : in std_logic_vector(3 downto 0);
        decrypt : in std_logic;
        hash : in std_logic;
        key_update : in std_logic;
        
        bdo : out std_logic_vector(31 downto 0);
        bdo_valid : out std_logic;
        bdo_ready : in std_logic;
        end_of_block : out std_logic;
        bdo_valid_bytes : out std_logic_vector(3 downto 0);
        bdo_type : out std_logic_vector(3 downto 0);
        
        msg_auth : out std_logic;
        msg_auth_valid : out std_logic;
        msg_auth_ready : in std_logic
    );
end controller;

architecture behavioral of controller is

    -- Controller states
    type controller_state is (IDLE, LOAD_KEY, LOAD_NPUB,
                              START_PERM, WAIT_PERM,
                              LOAD_BLK, LOAD_BLK_ZERO,
                              FINALIZE_DAT_OUT, OUTPUT_DAT_BLK, 
                              LOAD_TAG, OUTPUT_TAG, VERIFY_TAG,
                              START_PERM_INIT,
                              LOAD_HASH, LOAD_HASH_ZERO, 
                              FINALIZE_HASH_OUT, OUTPUT_HASH);
    signal current_state : controller_state;
    signal next_state : controller_state;
    
    -- Input/Output word counter
    signal word_cntr_en, word_cntr_init : std_logic;
    signal word_counter : integer;
    
    -- Signals to handle manipulation and storage of input words
    signal bdi_reg_en, key_reg_en : std_logic;
    signal bdi_pad_en, zero_fill : std_logic;

    -- Partial registers for storage of each word
    signal key_0, key_1, key_2, key_3: std_logic_vector(31 downto 0);
    signal bdi_p, bdi_z : std_logic_vector(31 downto 0);
    signal bdi_0, bdi_1, bdi_2, bdi_3, bdi_4, bdi_5, bdi_6, bdi_7 : std_logic_vector(31 downto 0);
    
    -- Complete data storage registers
    signal bdi_blk : std_logic_vector(255 downto 0);
    signal key_reg : std_logic_vector(127 downto 0);
    
    -- Signals to handle storage
    signal store_dec, dec_reg : std_logic;
    signal store_lblk, lblk_reg, eoi_reg : std_logic;
    signal bdi_valid_bytes_reg : std_logic_vector(3 downto 0);
    signal bdi_pad_reg, store_pad_en : std_logic;
    signal lword_index : integer;
    
    -- BDO output signals
    signal bdo_out_reg : std_logic_vector(255 downto 0);
    signal bdo_current : std_logic_vector(31 downto 0);
    signal valid_bytes_sel, bdo_en : std_logic;
    signal bdo_out_sel : std_logic_vector(1 downto 0);
    
    -- Datapath signals
    signal feistel_out : std_logic_vector(383 downto 0);                    -- Feistel unit
    signal rho_rate_in : std_logic_vector(255 downto 0);                    -- Rho 1
    signal rho_out : std_logic_vector(383 downto 0);
    signal inv_rho_out : std_logic_vector(383 downto 0);                    -- Inv rho 1
    signal rho_ct_out : std_logic_vector(383 downto 0);                     -- Rho 2
    signal padded_zero_pt : std_logic_vector(255 downto 0);                 -- Padded plaintext
    signal pad_const : std_logic_vector(31 downto 0);                       -- Pad constant
    signal inj_const_in, inj_const_out : std_logic_vector(383 downto 0);    -- Inject constant unit 
    signal rate_whiten_in, rate_whiten_out : std_logic_vector(383 downto 0);
    signal state_init_input : std_logic_vector(383 downto 0);
    signal tag : std_logic_vector(127 downto 0);
    
    -- Hash signals
    signal hash_feistel_in, hash_feistel_out, hash_constm, hash_state_in  : std_logic_vector(383 downto 0);
    signal hash_pad_const : std_logic_vector(31 downto 0);
    signal hash_pad_const_sel, fin_hash, lblk_in : std_logic;
    signal hash_init, store_hash_init, hash_init_val : std_logic;
    signal hash_value_d0 : std_logic_vector(127 downto 0);
    signal hash_value : std_logic_vector(255 downto 0);
    signal store_hash_d0, store_hash, hash_reg : std_logic;
    
    -- Datapath signal selects
    signal rho_rate_in_sel, inj_const_in_sel : std_logic;
    signal pad_const_sel : std_logic_vector(1 downto 0);
    signal ad_flag_in, ad_flag, store_ad_flag : std_logic;
    signal comp_tag : std_logic;
    
    -- Sparkle Permutation control signals: 
    signal perm_en, perm_complete : std_logic;
    signal num_steps : integer;
    signal state_sparkle_in, state_sparkle_out : std_logic_vector(383 downto 0);
    signal sparkle_in_sel : std_logic_vector(1 downto 0);
       
begin

    -- Registers:
        
    bdi_valid_reg_unit: entity work.regGen(behavioral)
    generic map (width => 4)
    port map(
        d => bdi_valid_bytes,
	    e => store_lblk,
	    clk => clk,
	    q => bdi_valid_bytes_reg
    );
        
    ad_flag_reg_unit: entity work.regOne(behavioral)
    port map(
        d => ad_flag_in,
	    e => store_ad_flag,
	    clk => clk,
	    q => ad_flag
    );
    
    dec_flag_reg_unit: entity work.regOne(behavioral)
    port map(
        d => decrypt,
	    e => store_dec,
	    clk => clk,
	    q => dec_reg
    );
    
    eoi_reg_unit: entity work.regOne(behavioral)
    port map(
        d => bdi_eoi,
	    e => store_lblk,
	    clk => clk,
	    q => eoi_reg
    );
    
    bdi_pad_reg_unit: entity work.regOne(behavioral)
    port map(
        d => bdi_pad_en,
	    e => store_pad_en,
	    clk => clk,
	    q => bdi_pad_reg
    );
    
    eot_reg_unit: entity work.regOne(behavioral)
    port map(
        d => lblk_in,
	    e => store_lblk,
	    clk => clk,
	    q => lblk_reg
    );
    
    lw_num_reg_unit: entity work.regNum(behavioral)
    port map(
        d => word_counter,
	    e => store_lblk,
	    clk => clk,
	    q => lword_index
    );
    
    h0_reg: entity work.regGen(behavioral)
    generic map (width => 128)
    port map(
        d => state_sparkle_out(383 downto 256),
	    e => store_hash_d0,
	    clk => clk,
	    q => hash_value_d0
    );
    
    hash_reg_unit: entity work.regOne(behavioral)
    port map(
        d => hash,
	    e => store_hash,
	    clk => clk,
	    q => hash_reg
    );
    
    hash_init_reg_unit: entity work.regOne(behavioral)
    port map(
        d => hash_init_val,
	    e => store_hash_init,
	    clk => clk,
	    q => hash_init
    );
        
    -- Datapath units: 

    fesitel_unit: entity work.feistel_swap(structural) 
    port map(
        state_in => state_sparkle_out,
        state_out => feistel_out
    ); 
    
    rho_state_unit: entity work.rho(structural)
    port map(
        state_in => feistel_out,      
        input_rate => rho_rate_in,
        state_out => rho_out
    ); 
    
    rho_ct_unit: entity work.rho(structural)
    port map(
        state_in => state_sparkle_out,      
        input_rate => bdi_blk,
        state_out => rho_ct_out
    );
    
    inv_rho_unit: entity work.inv_rho(structural)
    port map(
        state_in_pre_feistel => state_sparkle_out(383 downto 128),      
        state_in_post_feistel => feistel_out,      
        input_rate => bdi_blk,
        state_out => inv_rho_out
    );
    
    inject_const: entity work.inject_constant(structural)
    port map(
        state_in => inj_const_in,
        constant_value => pad_const,
        state_out => inj_const_out
    );
    
    rate_white_unit: entity work.rate_whitening(structural)
    port map(
        state_in => rate_whiten_in,
        state_out => rate_whiten_out
    ); 
    
    perm_fsm: entity work.sparkle_permutation_fsm(behavioral)
    port map (
        clk => clk,
        rst => rst,
        perm_start => perm_en,
        num_steps => num_steps,
        state_in => state_sparkle_in,
        state_out => state_sparkle_out,
        perm_complete => perm_complete
    );

    hash_feistel_function: entity work.feistel_function(structural)
        port map(
        state_in => hash_feistel_in,
        state_out => hash_feistel_out
        ); 
      
-- Handle BDI
bdi_z <= ZERO_W when (zero_fill = '1') else bdi;                                -- Zero fill bdi word if needed
bdi_p <= padWordLoc(bdi_z, bdi_pad_loc) when (bdi_pad_en = '1') else bdi_z;     -- Pad bdi word if needed

-- Assign input key, nonce, tag, ad, and dat registers
key_reg <= key_0 & key_1 & key_2 & key_3;
bdi_blk <= bdi_0 & bdi_1 & bdi_2 & bdi_3 & bdi_4 & bdi_5 & bdi_6 & bdi_7;

-- Assign the intialization state input
state_init_input <= bdi_blk & key_reg;
                             
-- Pad the computed plaintext to feed back into rho
padded_zero_pt <= zeroFillPt(rho_ct_out(383 downto 128), lword_index, bdi_valid_bytes_reg);

-- Calculate tag
tag <= state_sparkle_out(127 downto 0) xor key_reg;

hash_feistel_in <= bdi_blk(127 downto 0) & x"0000000000000000000000000000000000000000000000000000000000000000";
hash_constm <= (hash_feistel_out(383 downto 224) & (hash_feistel_out(223 downto 192) xor hash_pad_const) & hash_feistel_out(191 downto 0)) when (lblk_reg = '1') else hash_feistel_out;
hash_state_in <= hash_constm when (hash_init = '1') else (hash_constm xor state_sparkle_out);
lblk_in <= bdi_eot and fin_hash;

-- Assign hash value
hash_value <= hash_value_d0 & state_sparkle_out(383 downto 256); 

-- Flag to determine whether AD or DAT blocks are being processed 
ad_flag_in <= '1' when (bdi_type = HDR_AD) else '0';

-- MUX for sparkle input
with sparkle_in_sel select
state_sparkle_in <= state_init_input  when b"00", 
                    rate_whiten_out when b"01", 
                    hash_state_in when b"10", 
                    state_sparkle_out when b"11", 
                    state_init_input when others;
                    
-- MUX for rho state input
with rho_rate_in_sel select
rho_rate_in <= bdi_blk when '0',
               padded_zero_pt when '1',
               bdi_blk when others;

-- MUX for inject constant input       
with inj_const_in_sel select
inj_const_in <= rho_out when '0',
                inv_rho_out when '1',
                rho_out when others;

-- MUX for Sparkle number of steps
with lblk_reg select
num_steps <= STEPS_BIG when '1',
             STEPS_SMALL when '0',
             0 when others;

-- MUX for rate whiten input selection
with lblk_reg select
rate_whiten_in <= inj_const_in when '0', 
                  inj_const_out when '1', 
                  inj_const_in when others;

-- MUX for pad constant select
with pad_const_sel select
pad_const <= PAD_AD_CONST when b"00", 
             NO_PAD_AD_CONST when b"01", 
             PAD_PT_CONST when b"10", 
             NO_PAD_PT_CONST when b"11", 
             ZERO_W when others;

-- MUX for hash constant select
with hash_pad_const_sel select
hash_pad_const <= NO_PAD_HASH_CONST when '0', 
                  PAD_HASH_CONST when '1',
                  ZERO_W when others;

-- MUX for bdo output (dat, tag, or hash)
with bdo_out_sel select
bdo_out_reg <= rho_ct_out(383 downto 128) when b"00", 
               (tag & x"00000000000000000000000000000000") when b"01",
               hash_value when b"10",
               hash_value when others;

-- MUX for bdo output (which word)
with word_counter select
bdo_current <= bdo_out_reg(255 downto 224) when 0, 
       bdo_out_reg(223 downto 192) when 1,
       bdo_out_reg(191 downto 160) when 2,
       bdo_out_reg(159 downto 128) when 3,
       bdo_out_reg(127 downto 96) when 4,
       bdo_out_reg(95 downto 64) when 5,
       bdo_out_reg(63 downto 32) when 6,
       bdo_out_reg(31 downto 0) when 7,
       ZERO_W when others;
       
-- MUX for bdo output valid bytes (all valid or bdi valid reg)
with valid_bytes_sel select
bdo_valid_bytes <= VALID_WORD when '0', 
                   bdi_valid_bytes_reg when '1',
                   VALID_WORD when others;

register_input: process(clk)
begin
	if (rising_edge(clk)) then
        if (key_reg_en = '1') then
            key_3 <= littleEndianWord(key);
            key_2 <= key_3;
            key_1 <= key_2;
            key_0 <= key_1;
        end if;
        if (bdi_reg_en = '1') then
            bdi_7 <= littleEndianWord(bdi_p);
            bdi_6 <= bdi_7;
            bdi_5 <= bdi_6;
            bdi_4 <= bdi_5;
            bdi_3 <= bdi_4;
            bdi_2 <= bdi_3;
            bdi_1 <= bdi_2;
            bdi_0 <= bdi_1;
        end if;
        if (bdo_en = '1') then
            bdo <= littleEndianWord(bdo_current);
        end if;
    end if;
end process;

compare_tag: process(comp_tag, tag, bdi_blk)
begin
    msg_auth <= '0';                    -- Default
    if (comp_tag = '1') then            -- Perform tag comparison
        if (tag(127 downto 96) = bdi_blk(127 downto 96)) and (tag(95 downto 64) = bdi_blk(95 downto 64)) and
           (tag(63 downto 32) = bdi_blk(63 downto 32)) and (tag(31 downto 0) = bdi_blk(31 downto 0)) then
            msg_auth <= '1';
        end if;
	end if;
end process;

counter_process: process(clk)
begin
    if (rising_edge(clk)) then
        if (word_cntr_en = '1') then
            if (word_cntr_init = '1') then
                word_counter <= 0;
            else
                word_counter <= word_counter + 1;
            end if;
        else 
            word_counter <= 0;
		end if;
	end if;
end process;
	
sync_process: process(clk)
begin
    if (rising_edge(clk)) then
        if (rst = '1') then
           current_state <= IDLE;
        else
           current_state <= next_state;
        end if;
    end if;
end process;

fsm_process: process(current_state, key_update, bdi_valid, perm_complete, word_counter, bdi_eot)
begin
 
    -- DEFAULTS:
    next_state <= current_state;            -- Default return to current state 
    perm_en <= '0';                         -- Sparkle permutation start flag
    comp_tag <= '0';                        -- Signal to enable tag comparison
    
    bdo_en <= '0';                          -- Output to postprocessor
    msg_auth_valid <= '0';
    end_of_block <= '0';
    bdo_valid <= '0';
    
    key_ready <= '0';                       -- Output to preprocessor
    bdi_ready <= '0';
    
    bdi_pad_en <= '0';                      -- BDI/SDI signals
    zero_fill <= '0';
    bdi_reg_en <= '0';
    key_reg_en <= '0';
    
    word_cntr_init <= '0';                  -- Word counter
    word_cntr_en <= '0';
    
    store_lblk <= '0';                      -- Signals to enable storage
    store_dec <= '0';
    store_ad_flag <= '0';
    store_pad_en <= '0';
    
    rho_rate_in_sel <= '0';                 -- MUX select signals
    inj_const_in_sel <= '0';
    pad_const_sel <= b"00";
    sparkle_in_sel <= b"00";
    bdo_out_sel <= b"00";
    valid_bytes_sel <= '0';
    
    store_hash <= '0';                      -- Hash signals
    store_hash_d0 <= '0';
    store_hash_init <= '0';
    hash_init_val <= '0';
    hash_pad_const_sel <= '0';
    fin_hash <= '1';
        
    case current_state is
                     
        when IDLE => 
            store_pad_en <= '1';
            if (key_update = '1') then
                if (key_valid = '1') then
                    next_state <= LOAD_KEY;
                end if;
            elsif (bdi_valid = '1') then
                if (bdi_type = HDR_NPUB) then
                    next_state <= LOAD_NPUB;
                elsif (bdi_type = HDR_HASH_MSG) then
                    store_hash_init <= '1';
                    hash_init_val <= '1';
                    store_hash <= '1';                          -- Store hash signal
                    next_state <= LOAD_HASH;
                end if;
            end if;
        
        when LOAD_KEY => 
            key_ready <= '1';                           -- Set output key ready signal
            key_reg_en <= '1';                          -- Enable storage of each word
            word_cntr_en <= '1';                        -- Keep word counter enabled while loading key
            
            if (word_counter = KEY_SIZE - 1) then
                word_cntr_init <= '1';                  -- Reset counter value to 0
                next_state <= IDLE;                     -- Return to IDLE to wait for NPUB
            end if;
        
        when LOAD_NPUB => 
            bdi_ready <= '1';                           -- Set output bdi ready signal
            bdi_reg_en <= '1';                          -- Enable storage of each word
            word_cntr_en <= '1';                        -- Keep word counter enabled while loading NPUB
            store_lblk <= '1';                          -- Enable storage of last block
            
            if (word_counter = BLK_SIZE - 1) then
                word_cntr_init <= '1';                  -- Reset counter value to 0
                store_dec <= '1';                       -- Enable storage of decrypt flag
                next_state <= START_PERM_INIT;
            end if;
        
        when START_PERM_INIT =>                         -- Handle starting permutation for initialization
            perm_en <= '1';                             -- Start permutation
            next_state <= WAIT_PERM;                    -- Update state to wait for completion
            
        when WAIT_PERM => 
            if (perm_complete = '1') then               -- Wait for completion
                if (hash_reg = '1') then
                    if (bdi_valid = '1') and (bdi_size /= b"00") and (lblk_reg /= '1') then
                        next_state <= LOAD_HASH;
                    else
                        next_state <= FINALIZE_HASH_OUT;
                        sparkle_in_sel <= b"11";
                        fin_hash <= '0';
                        perm_en <= '1';
                        store_hash_d0 <= '1';           -- Store the state for use in finalization
                        store_lblk <= '1';              -- Enable last block to reset to STEPS_SMALL for finalization
                    end if;
                elsif (eoi_reg = '1') then              -- If end of input, handle tag based on enc or dec
                    if (dec_reg = '1') then
                        next_state <= LOAD_TAG;         -- If dec, then load tag from input
                    else
                        next_state <= OUTPUT_TAG;       -- If enc, then transition to outputting calculated tag
                        bdo_out_sel <= b"01";             -- Select TAG for BDO output
                        word_cntr_en <= '1';            -- Enable word counter
                        bdo_en <= '1';                  -- Enable bdo output
                    end if;
                else                                    -- If NOT end of input, transition to loading AD or DAT
                    if (bdi_valid = '1') then
                        next_state <= LOAD_BLK;
                        store_ad_flag <= '1';           -- Store whether input is AD or DAT
                        store_pad_en <= '1';            -- Reset padding
                    end if;
                end if;
            end if;
              
        when LOAD_BLK => 
            bdi_ready <= '1';                           -- Set output bdi ready signal
            bdi_reg_en <= '1';                          -- Enable storage of each word
            word_cntr_en <= '1';                        -- Keep word counter enabled while loading blk
            store_lblk <= '1';                          -- Enable storage of last block
            
            -- Handle padding of current word
            if (bdi_valid_bytes /= VALID_WORD) then
                bdi_pad_en <= '1';                      -- If the current block is not all valid, enable padding
                store_pad_en <= '1';
            end if;
            
            -- Handle end of input block
            if (word_counter = BLK_SIZE - 1) then       -- Full block loaded
                word_cntr_init <= '1';                  -- Reset counter value to 0
                if (ad_flag = '1') then                 -- If handling AD, start permutation
                    next_state <= START_PERM;
                else
                    next_state <= FINALIZE_DAT_OUT;     -- If handling DAT, finalize output
                end if;
            elsif (bdi_eot = '1') then                  -- Block still loading, handle incomplete last input block
                next_state <= LOAD_BLK_ZERO;            -- Update state to zero fill
            end if;
        
        when LOAD_BLK_ZERO => 
            bdi_reg_en <= '1';                          -- Enable storage of each word
            word_cntr_en <= '1';                        -- Keep word counter enabled while loading ad
            zero_fill <= '1';                           -- Enable zero fill for the rest of block
            
            -- Check previous word validity
            if (bdi_valid_bytes = VALID_WORD) then
                bdi_pad_en <= '1';                      -- Enable padding of zero-filled word
                store_pad_en <= '1';                    -- Store the padding flag
            end if;

            if (word_counter = BLK_SIZE - 1) then       -- Full block loaded
                word_cntr_init <= '1';                  -- Reset counter value to 0
                if (ad_flag = '1') then                 -- If handling AD, start permutation
                    next_state <= START_PERM;
                else
                    next_state <= FINALIZE_DAT_OUT;     -- If handling DAT, finalize output
                end if;
            end if;
                    
        when START_PERM =>
            store_hash_init <= '1';
            if (hash_reg = '1') then
                sparkle_in_sel <= b"10";
            else
                sparkle_in_sel <= b"01";                -- Select rate whitening output as state input
            end if;
            perm_en <= '1';                             -- Start permutation
            next_state <= WAIT_PERM;                    -- Update state to wait for completion

            -- Select the correct pad constant
            if (ad_flag = '1') then
                if (bdi_pad_reg = '1') then
                    pad_const_sel <= b"00";             -- Update pad constant select: PAD AD
                else
                    pad_const_sel <= b"01";             -- Update pad constant select: NO PAD AD
                end if;
            else
                if (bdi_pad_reg = '1') then
                    pad_const_sel <= b"10";             -- Update pad constant select: PAD DAT
                else
                    pad_const_sel <= b"11";             -- Update pad constant select: NO PAD DAT
                end if;
            end if;
            
            if (bdi_pad_reg = '1') then
                hash_pad_const_sel <= '1';             -- Update pad constant select: PAD HASH
            end if;
            
            -- If decrypting completely valid full block use inv rho, else use rho with padded PT input
            -- Must be handling DAT blocks
            if (dec_reg = '1') then
                if (ad_flag /= '1') then
                    if (bdi_pad_reg = '0') then
                        inj_const_in_sel <= '1';
                    else
                        rho_rate_in_sel <= '1';
                    end if;
                end if;
            end if;
            
        when FINALIZE_DAT_OUT => 
            bdo_en <= '1';                              -- Enable output
            word_cntr_en <= '1';                        -- Enable word counter
            next_state <= OUTPUT_DAT_BLK;
            
        when OUTPUT_DAT_BLK => 
            bdo_en <= '1';                              -- Enable output
            word_cntr_en <= '1';                        -- Keep word counter enabled while outputting data
            bdo_valid <= '1';
   
            if (word_counter = lword_index + 1) then    -- End of block
                bdo_en <= '0';                          -- Enable output
                valid_bytes_sel <= '1';                 -- Select bdi valid bytes reg for last word 
                end_of_block <= '1';                    -- Indicate end of output block
                word_cntr_init <= '1';                  -- Reset counter value to 0
                next_state <= START_PERM;
            end if;
            
        when LOAD_TAG => 
            bdi_ready <= '1';                           -- Set output bdi ready signal
            bdi_reg_en <= '1';                          -- Enable storage of each word
            word_cntr_en <= '1';                        -- Keep word counter enabled while loading tag
            
            if (word_counter = TAG_SIZE - 1) then
                word_cntr_init <= '1';                  -- Reset counter value to 0
                next_state <= VERIFY_TAG;
            end if;
                      
        when OUTPUT_TAG =>            
            bdo_out_sel <= b"01";                         -- Select TAG for BDO output
            bdo_en <= '1';                              -- Enable output
            word_cntr_en <= '1';                        -- Keep word counter enabled while outputting data
            bdo_valid <= '1';
                        
            if (word_counter = TAG_SIZE) then          
                end_of_block <= '1';                    -- Indicate end of output tag block
                word_cntr_init <= '1';                  -- Reset counter value to 0
                next_state <= IDLE;
            end if;
        
        when VERIFY_TAG => 
            msg_auth_valid <= '1';                      -- Indicate msg auth output is valid
            comp_tag <= '1';                            -- Enable tag comparison
            next_state <= IDLE;                         -- Return to IDLE state
            
        when LOAD_HASH => 
            bdi_ready <= '1';                           -- Set output bdi ready signal
            bdi_reg_en <= '1';                          -- Enable storage of each word
            word_cntr_en <= '1';                        -- Keep word counter enabled while loading blk
            store_lblk <= '1';                          -- Enable storage of last block
            
            -- Handle padding of current word
            if (bdi_valid_bytes /= VALID_WORD) then
                bdi_pad_en <= '1';                      -- If the current block is not all valid, enable padding
                store_pad_en <= '1';
            end if;
            
            -- Handle end of input block
            if (word_counter = HASH_BLK_SIZE - 1) then  -- Full block loaded
                word_cntr_init <= '1';                  -- Reset counter value to 0
                next_state <= START_PERM;
            elsif (bdi_eot = '1') then                  -- Block still loading, handle incomplete last input block
                next_state <= LOAD_HASH_ZERO;            -- Update state to zero fill
            end if;
        
        when LOAD_HASH_ZERO => 
            bdi_reg_en <= '1';                          -- Enable storage of each word
            word_cntr_en <= '1';                        -- Keep word counter enabled while loading ad
            zero_fill <= '1';                           -- Enable zero fill for the rest of block
            
            -- Check previous word validity
            if (bdi_valid_bytes = VALID_WORD) then
                bdi_pad_en <= '1';                      -- Enable padding of zero-filled word
                store_pad_en <= '1';                    -- Store the padding flag
            end if;

            if (word_counter = HASH_BLK_SIZE - 1) then       -- Full block loaded
                word_cntr_init <= '1';                  -- Reset counter value to 0
                next_state <= START_PERM;
            end if;
        
        when FINALIZE_HASH_OUT => 
            fin_hash <= '0';
            sparkle_in_sel <= b"11";                    -- Select state output for sparkle input
            if (perm_complete = '1') and (num_steps = 7) then     
                bdo_out_sel <= b"10";                   -- Select hash for BDO output
                bdo_en <= '1';                          -- Enable output
                word_cntr_en <= '1';                    -- Enable word counter
                next_state <= OUTPUT_HASH;
            end if;
        
        when OUTPUT_HASH =>       
            bdo_out_sel <= b"10";                       -- Select hash for BDO output
            bdo_en <= '1';                              -- Enable output
            bdo_valid <= '1';                           -- Set bdo valid flag
            word_cntr_en <= '1';                        -- Keep word counter enabled while outputting hash
            store_hash <= '1';
            
            if (word_counter = HASH_SIZE) then
                end_of_block <= '1';                    -- Indicate end of output tag block
                word_cntr_init <= '1';                  -- Reset counter value to 0
                next_state <= IDLE;
            end if;
            
        when others =>
            next_state <= IDLE;
            
    end case; 

end process;
end behavioral;
