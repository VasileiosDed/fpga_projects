# fpga_projects
# Lab2_FPGA

# Programm 1

library ieee;
use ieee.std_logic_1164.all;

entity adder1bit is
    port (
        a    : in  std_logic;
        b    : in  std_logic;
        cin  : in  std_logic;
        s    : out std_logic;
        cout : out std_logic
    );
end adder1bit;

architecture behavior of adder1bit is
begin
    s    <= a xor b xor cin;
    cout <= (a and b) or (b and cin) or (a and cin);
end behavior;

# Programm 2

library ieee;
use ieee.std_logic_1164.all;

entity adder8bit is
    port (
        a    : in  std_logic_vector(7 downto 0);
        b    : in  std_logic_vector(7 downto 0);
        cin  : in  std_logic;
        s    : out std_logic_vector(7 downto 0);
        cout : out std_logic
    );
end adder8bit;

architecture structural of adder8bit is
    component adder1bit
        port (
            a    : in  std_logic;
            b    : in  std_logic;
            cin  : in  std_logic;
            s    : out std_logic;
            cout : out std_logic
        );
    end component;

    signal carry : std_logic_vector(8 downto 0);
begin
    carry(0) <= cin;

    gen_adders: for i in 0 to 7 generate
        adder_inst: adder1bit
            port map (
                a    => a(i),
                b    => b(i),
                cin  => carry(i),
                s    => s(i),
                cout => carry(i+1)
            );
    end generate;

    cout <= carry(8);
end structural;

# Programm 3

library ieee;
use ieee.std_logic_1164.all;

entity mux2 is
    port (
        a   : in  std_logic_vector(7 downto 0);
        b   : in  std_logic_vector(7 downto 0);
        sel : in  std_logic;
        y   : out std_logic_vector(7 downto 0)
    );
end mux2;

architecture behavior of mux2 is
begin
    y <= a when sel = '0' else b;
end behavior;

# Programm 4

library ieee;
use ieee.std_logic_1164.all;

entity mux4 is
    port (
        a   : in  std_logic_vector(7 downto 0);
        b   : in  std_logic_vector(7 downto 0);
        c   : in  std_logic_vector(7 downto 0);
        d   : in  std_logic_vector(7 downto 0);
        sel : in  std_logic_vector(1 downto 0);
        y   : out std_logic_vector(7 downto 0)
    );
end mux4;

architecture behavior of mux4 is
begin
    process(a, b, c, d, sel)
    begin
        case sel is
            when "00" => y <= a;
            when "01" => y <= b;
            when "10" => y <= c;
            when "11" => y <= d;
            when others => y <= (others => '0');
        end case;
    end process;
end behavior;

# Programm 5

library ieee;
use ieee.std_logic_1164.all;

package alulib is

    component adder8bit
        port (
            a    : in  std_logic_vector(7 downto 0);
            b    : in  std_logic_vector(7 downto 0);
            cin  : in  std_logic;
            s    : out std_logic_vector(7 downto 0);
            cout : out std_logic
        );
    end component;

    component mux2
        port (
            a   : in  std_logic_vector(7 downto 0);
            b   : in  std_logic_vector(7 downto 0);
            sel : in  std_logic;
            y   : out std_logic_vector(7 downto 0)
        );
    end component;

    component mux4
        port (
            a   : in  std_logic_vector(7 downto 0);
            b   : in  std_logic_vector(7 downto 0);
            c   : in  std_logic_vector(7 downto 0);
            d   : in  std_logic_vector(7 downto 0);
            sel : in  std_logic_vector(1 downto 0);
            y   : out std_logic_vector(7 downto 0)
        );
    end component;

end package alulib;

# Programm 6

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use work.alulib.all;

entity alu is
    generic (n : integer := 8);
    port (
        ac    : in  std_logic_vector(n-1 downto 0);
        db    : in  std_logic_vector(n-1 downto 0);
        alus  : in  std_logic_vector(7 downto 1);
        dout  : out std_logic_vector(n-1 downto 0)
    );
end alu;

architecture arch of alu is
    signal add_res, sub_res : std_logic_vector(n-1 downto 0);
    signal cout             : std_logic;
    signal logic_res        : std_logic_vector(n-1 downto 0);
    signal not_ac           : std_logic_vector(n-1 downto 0);
    signal clac             : std_logic_vector(n-1 downto 0) := (others => '0');
    signal inac             : std_logic_vector(n-1 downto 0);
    signal movr             : std_logic_vector(n-1 downto 0);
    signal ldac             : std_logic_vector(n-1 downto 0);

begin
    -- Αριθμητικές πράξεις
    add_unit: adder8bit port map(ac, db, '0', add_res, cout);
    sub_unit: adder8bit port map(ac, not db, '1', sub_res, cout); -- SUB = A + (~B + 1)

    -- Λογικές πράξεις
    not_ac <= not ac;
    inac   <= ac + 1;
    movr   <= db;
    ldac   <= (others => '0');

    process(ac, db, alus, add_res, sub_res, not_ac, inac)
    begin
        case alus is
            when "1000000" => dout <= ac and db;         -- AND
            when "1100000" => dout <= ac or db;          -- OR
            when "1010000" => dout <= ac xor db;         -- XOR
            when "1110000" => dout <= not_ac;            -- NOT
            when "0000000" => dout <= clac;              -- CLAC (clear ac)
            when "0001001" => dout <= inac;              -- INAC
            when "0000100" => dout <= movr;              -- MOVR
            when "0000101" => dout <= add_res;           -- ADD
            when "0001011" => dout <= sub_res;           -- SUB
            when others    => dout <= (others => '0');   -- Default
        end case;
    end process;

end arch;

# fpga_projects
# Lab2_FPGA

Programma 1

library ieee;
use ieee.std_logic_1164.all;

entity regn is
  generic ( N : integer := 6 );
  port (
    clk   : in  std_logic;
    reset : in  std_logic;
    load  : in  std_logic;
    d     : in  std_logic_vector(N-1 downto 0);
    q     : out std_logic_vector(N-1 downto 0)
  );
end regn;

architecture beh of regn is
  signal q_reg : std_logic_vector(N-1 downto 0);
begin
  process(clk, reset)
  begin
    if reset = '1' then
      q_reg <= (others => '0');
    elsif rising_edge(clk) then
      if load = '1' then
        q_reg <= d;
      end if;
    end if;
  end process;

  q <= q_reg;
end beh;

Programma 2

library ieee;
use ieee.std_logic_1164.all;

entity mux4_1 is
  generic ( N : integer := 6 );
  port (
    s        : in  std_logic_vector(1 downto 0);
    a0, a1   : in  std_logic_vector(N-1 downto 0);
    a2, a3   : in  std_logic_vector(N-1 downto 0);
    y        : out std_logic_vector(N-1 downto 0)
  );
end mux4_1;

architecture beh of mux4_1 is
begin
  process(s, a0, a1, a2, a3)
  begin
    case s is
      when "00" => y <= a0;  -- διεύθυνση από πεδίο ADDR
      when "01" => y <= a1;  -- διεύθυνση από IR (opcode*4)
      when "10" => y <= a2;  -- επόμενη μικροδιεύθυνση (uaddr + 1)
      when others => y <= a3; -- διεύθυνση FETCH1 (1)
    end case;
  end process;
end beh;

 Programma 3

 library ieee;
use ieee.std_logic_1164.all;

package mseqlib is

  -- n-bit καταχωρητής
  component regn
    generic ( N : integer := 6 );
    port (
      clk   : in  std_logic;
      reset : in  std_logic;
      load  : in  std_logic;
      d     : in  std_logic_vector(N-1 downto 0);
      q     : out std_logic_vector(N-1 downto 0)
    );
  end component;

  -- Πολυπλέκτης 4-σε-1 n-bit
  component mux4_1
    generic ( N : integer := 6 );
    port (
      s        : in  std_logic_vector(1 downto 0);
      a0, a1   : in  std_logic_vector(N-1 downto 0);
      a2, a3   : in  std_logic_vector(N-1 downto 0);
      y        : out std_logic_vector(N-1 downto 0)
    );
  end component;

  -- Μνήμη μικροκώδικα που δημιουργείς με τον MegaWizard (mseq_rom.vhd)
  component mseq_rom
    port (
      address : in  std_logic_vector(5 downto 0);
      clock   : in  std_logic;
      q       : out std_logic_vector(35 downto 0)
    );
  end component;

end mseqlib;

Programma 4

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

library lpm;
use lpm.lpm_components.all;

use work.mseqlib.all;

entity mseq is
  port(
    ir          : in  std_logic_vector(3 downto 0);
    clock       : in  std_logic;
    reset       : in  std_logic;
    z           : in  std_logic;
    code        : out std_logic_vector(35 downto 0);
    mOPs        : out std_logic_vector(26 downto 0)
  );
end mseq;

architecture arc of mseq is
  signal uaddr      : std_logic_vector(5 downto 0);
  signal nextaddr   : std_logic_vector(5 downto 0);
  signal code_int   : std_logic_vector(35 downto 0);
  signal sel        : std_logic_vector(2 downto 0);
  signal addr_field : std_logic_vector(5 downto 0);
  signal seq_addr   : std_logic_vector(5 downto 0);
  signal opc_addr   : std_logic_vector(5 downto 0);
  signal fetch_addr : std_logic_vector(5 downto 0);
  signal s          : std_logic_vector(1 downto 0);
begin

  uPC_reg : regn
    generic map ( N => 6 )
    port map (
      clk   => clock,
      reset => reset,
      load  => '1',
      d     => nextaddr,
      q     => uaddr
    );

  rom_inst : mseq_rom
    port map (
      address => uaddr,
      clock   => clock,
      q       => code_int
    );

  sel        <= code_int(35 downto 33);
  mOPs       <= code_int(32 downto 6);
  addr_field <= code_int(5 downto 0);
  code       <= code_int;

  seq_addr   <= uaddr + "000001";
  opc_addr   <= ir & "00";
  fetch_addr <= "000001";

  process(sel, z)
  begin
    case sel is
      when "000" =>
        s <= "00";
      when "001" =>
        s <= "01";
      when "010" =>
        if z = '0' then
          s <= "00";
        else
          s <= "10";
        end if;
      when "100" =>
        if z = '1' then
          s <= "00";
        else
          s <= "10";
        end if;
      when "110" =>
        s <= "11";
      when others =>
        s <= "10";
    end case;
  end process;

  next_mux : mux4_1
    generic map ( N => 6 )
    port map (
      s  => s,
      a0 => addr_field,
      a1 => opc_addr,
      a2 => seq_addr,
      a3 => fetch_addr,
      y  => nextaddr
    );

end arc;

# fpga_projects
# Lab4_FPGA

# Programma 1

library ieee;
use ieee.std_logic_1164.all;

entity instr_decoder is
  port(
    Din  : in  std_logic_vector(3 downto 0);
    Dout : out std_logic_vector(15 downto 0)
  );
end instr_decoder;

architecture beh of instr_decoder is
begin
  process(Din)
  begin
    Dout <= (others => '0');
    case Din is
      when "0000" => Dout(0)  <= '1';
      when "0001" => Dout(1)  <= '1';
      when "0010" => Dout(2)  <= '1';
      when "0011" => Dout(3)  <= '1';
      when "0100" => Dout(4)  <= '1';
      when "0101" => Dout(5)  <= '1';
      when "0110" => Dout(6)  <= '1';
      when "0111" => Dout(7)  <= '1';
      when "1000" => Dout(8)  <= '1';
      when "1001" => Dout(9)  <= '1';
      when "1010" => Dout(10) <= '1';
      when "1011" => Dout(11) <= '1';
      when "1100" => Dout(12) <= '1';
      when "1101" => Dout(13) <= '1';
      when "1110" => Dout(14) <= '1';
      when others => Dout(15) <= '1';
    end case;
  end process;
end beh;

# Programma 2

library ieee;
use ieee.std_logic_1164.all;

entity state_decoder is
  port(
    Din  : in  std_logic_vector(2 downto 0);
    Dout : out std_logic_vector(7 downto 0)
  );
end state_decoder;

architecture beh of state_decoder is
begin
  process(Din)
  begin
    Dout <= (others => '0');
    case Din is
      when "000" => Dout(0) <= '1';
      when "001" => Dout(1) <= '1';
      when "010" => Dout(2) <= '1';
      when "011" => Dout(3) <= '1';
      when "100" => Dout(4) <= '1';
      when "101" => Dout(5) <= '1';
      when "110" => Dout(6) <= '1';
      when others => Dout(7) <= '1';
    end case;
  end process;
end beh;

# Programma 3

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity counter3 is
  port(
    clock : in  std_logic;
    rst   : in  std_logic;
    inc   : in  std_logic;
    count : out std_logic_vector(2 downto 0)
  );
end counter3;

architecture beh of counter3 is
  signal cnt : std_logic_vector(2 downto 0);
begin
  process(clock, rst)
  begin
    if rst = '1' then
      cnt <= "000";
    elsif rising_edge(clock) then
      if inc = '1' then
        cnt <= cnt + 1;
      end if;
    end if;
  end process;

  count <= cnt;
end beh;

# Programma 4

library ieee;
use ieee.std_logic_1164.all;

package hardwiredlib is

  component instr_decoder
    port(
      Din  : in  std_logic_vector(3 downto 0);
      Dout : out std_logic_vector(15 downto 0)
    );
  end component;

  component state_decoder
    port(
      Din  : in  std_logic_vector(2 downto 0);
      Dout : out std_logic_vector(7 downto 0)
    );
  end component;

  component counter3
    port(
      clock : in  std_logic;
      rst   : in  std_logic;
      inc   : in  std_logic;
      count : out std_logic_vector(2 downto 0)
    );
  end component;

end hardwiredlib;

# Programma 5

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

library lpm;
use lpm.lpm_components.all;

use work.hardwiredlib.all;

entity hardwired is
  port(
    ir             : in  std_logic_vector(3 downto 0);
    clock, reset   : in  std_logic;
    z              : in  std_logic;
    mOPs           : out std_logic_vector(26 downto 0)
  );
end hardwired;

architecture arc of hardwired is

  signal I     : std_logic_vector(15 downto 0);
  signal T     : std_logic_vector(7 downto 0);
  signal count : std_logic_vector(2 downto 0);
  signal inc, clr : std_logic;

begin

  ID : instr_decoder port map(ir, I);
  SD : state_decoder port map(count, T);
  CNT : counter3 port map(clock, clr, inc, count);

  clr <= T(7) or T(6);
  inc <= not clr;

  mOPs <= (others => '0');

end arc;

