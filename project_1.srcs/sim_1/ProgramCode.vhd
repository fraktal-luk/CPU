----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 04.11.2018 22:48:18
-- Design Name: 
-- Module Name: ProgramCode - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
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

use work.BasicTypes.all;
use work.ArchDefs.all;
	
--use work.Decoding2.all;

package ProgramCode is

		subtype WordMem is WordArray (0 to 511 + 512);
		
		constant insRET: word := ins655655(ext1, r0, r0, jzR, r31, 0); -- j to r31
		constant insERROR: word := ins655655(ext2, 0, 0, error, 0, 0);		
		constant insNOP: word := ins655H(addI, r0, r0, 0);
		function insCLEAR(reg: integer) return Word;
		function insSET(reg, num: integer) return Word;		
		function insMOVE(rd, rs: integer) return Word;

		function insSTORE(ra, rb, num: integer) return Word;
		function insLOAD(ra, rb, num: integer) return Word;		

		
		-- Address of handling function
		constant ERR_HANDLER: integer := 4000;
		
		constant testProg1: WordMem := ( -- mem load testing 
			0 => insNOP, --
					--	ins655H(addI, r2, r0, 10000),
			1 => insNOP, --
					--	ins655H(addI, r3, r0, 200),
			2 => --insNOP,
					ins65J(jl, r31, 4*(430-2)),
					--	ins655655(ext0, r31, r2, mulu, r3, 0),
					--ins655655(ext2, 0, 0, halt, 0, 0),
			3 => insNOP,			
						
			4 => ins65J(jl, r31, 4*(320-4)), -- Test result forwarding src1
			5 => ins65J(jl, r31, 4*(350-5)), -- Test result forwarding src0
			6 => ins65J(jl, r31, 4*(380-6)), -- Test 0+1 forwarding
			
			7 => ins65J(jl, r31, 4*(240-7)), -- Store registers to 4*(0:31)
			8 => insSET(r4, 4*16),					-- Arg in r4 for function call
			9 => ins65J(jl, r31, 4*(280-9)), -- Load registers from 4*(16:48)
			
			10 => ins65J(jz, r0, 4* (10)), -- Jump to ins 20 (@80)
			
			20 => insNOP,-- ins65J(jl, r31, 4*(320-20)), -- Test result forwarding src1
			21 => X"ffffffff", --ins65J(jl, r31, 4*(350-21)), -- Test result forwarding src0
			22 => insNOP,--ins65J(jl, r31, 4*(380-22)), -- Test 0+1 forwarding 
			
			-- Check sysReg storage
			23 => insSET(r25, 491),
			24 => ins6556X(ext2, r25, 0, mtc, 2),
				25 => ins655655(ext2, 0, 0, sync, 0, 0),
			26 => ins6556X(ext2, r26, 0, mfc, 2),
			27 => ins655655(ext0, r25, r25, subR, r26, 0),
			28 => ins65J(jnz, r25, 4*(1023 - 28)), -- if not, jump to illegal addr
			29 => --insNOP,
					ins65J(jl, r31, 4*(410-29)), -- Test store to load forwarding
					--ins655655(ext0, r0, r26, shlC, 5, 5), -- Shifts
			30 => insNOP,
					--ins655655(ext0, r0, r26, shraC, 5, 5),
			31 => insNOP,
					--ins655655(ext0, r27, r26, shlC, 23, 5),
			32 => insNOP,
					--ins655655(ext0, r0, r27, shraC, 31, 5),
			
			33 => ins65J(jz, r0, 4* (-30)), -- jump to 3(@12)
			
			-- On expception
			-- @256
			64 => insNOP, --		X"ffffffff",  -- == 256/4 -> exc handler
			65 => insNOP,
			66 => ins6556X(ext2, r20, 0, mfc, 2), -- 2: ELR
			67 => ins6556X(ext2, r21, 0, mfc, 4), -- 4: Exc saved state
			68 => insNOP,--ins6556X(ext2, r21, 0, mtc, 1),   -- 1: current state
				69 => ins655655(ext2, 0, 0, sync, 0, 0),
			70 => --ins655655(ext1, r0, r0, jzR, r20, 0),	-- Jump to saved link address
					ins655655(ext2, 0, 0, retE, 0, 0), -- Proper return instruction
			
			-- On interrupt
			-- @512
			128 => insNOP,
			129 => insNOP,	
				130 => insNOP,
			131 => ins6556X(ext2, r20, 0, mfc, 3), -- 3: ILR
			132 => ins6556X(ext2, r21, 0, mfc, 5), -- 5: Int saved state
			133 => insNOP,--ins6556X(ext2, r21, 0, mtc, 1),   -- 1: current state
				134 => ins655655(ext2, 0, 0, sync, 0, 0),			
			135 => --ins655655(ext1, r0, r0, jzR, r20, 0),	-- Jump to saved link address
					ins655655(ext2, 0, 0, retI, 0, 0), -- Proper return instruction
			
			-- Clear registers
			-- @800
			200 => ins655H(addI, r0, r0, 0),
			201 => ins655H(addI, r1, r0, 0),
			202 => ins655H(addI, r2, r0, 0),
			203 => ins655H(addI, r3, r0, 0),
			204 => ins655H(addI, r4, r0, 0),
			205 => ins655H(addI, r5, r0, 0),
			206 => ins655H(addI, r6, r0, 0),
			207 => ins655H(addI, r7, r0, 0),
			208 => ins655H(addI, r8, r0, 0),
			209 => ins655H(addI, r9, r0, 0),
			210 => ins655H(addI, r10, r0, 0),
			211 => ins655H(addI, r11, r0, 0),
			212 => ins655H(addI, r12, r0, 0),
			213 => ins655H(addI, r13, r0, 0),
			214 => ins655H(addI, r14, r0, 0),
			215 => ins655H(addI, r15, r0, 0),
			216 => ins655H(addI, r16, r0, 0),
			217 => ins655H(addI, r17, r0, 0),
			218 => ins655H(addI, r18, r0, 0),
			219 => ins655H(addI, r19, r0, 0),
			220 => ins655H(addI, r20, r0, 0),
			221 => ins655H(addI, r21, r0, 0),
			222 => ins655H(addI, r22, r0, 0),
			223 => ins655H(addI, r23, r0, 0),
			224 => ins655H(addI, r24, r0, 0),
			225 => ins655H(addI, r25, r0, 0),
			226 => ins655H(addI, r26, r0, 0),
			227 => ins655H(addI, r27, r0, 0),
			228 => ins655H(addI, r28, r0, 0),
			229 => ins655H(addI, r29, r0, 0),
			230 => ins655H(addI, r30, r0, 0),
			231 => ins655H(addI, r31, r0, 0),
			232 => insRET, -- return
			
			-- Store registers in mem (word*)0:31
			-- @960
			240 => ins6556X(ext1, r0, r0, store, 4*0),
			241 => ins6556X(ext1, r1, r0, store, 4*1),
			242 => ins6556X(ext1, r2, r0, store, 4*2),
			243 => ins6556X(ext1, r3, r0, store, 4*3),
			244 => ins6556X(ext1, r4, r0, store, 4*4),
			245 => ins6556X(ext1, r5, r0, store, 4*5),
			246 => ins6556X(ext1, r6, r0, store, 4*6),
			247 => ins6556X(ext1, r7, r0, store, 4*7),
			248 => ins6556X(ext1, r8, r0, store, 4*8),
			249 => ins6556X(ext1, r9, r0, store, 4*9),
			250 => ins6556X(ext1, r10, r0, store, 4*10),
			251 => ins6556X(ext1, r11, r0, store, 4*11),
			252 => ins6556X(ext1, r12, r0, store, 4*12),
			253 => ins6556X(ext1, r13, r0, store, 4*13),
			254 => ins6556X(ext1, r14, r0, store, 4*14),
			255 => ins6556X(ext1, r15, r0, store, 4*15),			
			256 => ins6556X(ext1, r16, r0, store, 4*16),
			257 => ins6556X(ext1, r17, r0, store, 4*17),
			258 => ins6556X(ext1, r18, r0, store, 4*18),
			259 => ins6556X(ext1, r19, r0, store, 4*19),
			260 => ins6556X(ext1, r20, r0, store, 4*20),
			261 => ins6556X(ext1, r21, r0, store, 4*21),
			262 => ins6556X(ext1, r22, r0, store, 4*22),
			263 => ins6556X(ext1, r23, r0, store, 4*23),
			264 => ins6556X(ext1, r24, r0, store, 4*24),
			265 => ins6556X(ext1, r25, r0, store, 4*25),
			266 => ins6556X(ext1, r26, r0, store, 4*26),
			267 => ins6556X(ext1, r27, r0, store, 4*27),
			268 => ins6556X(ext1, r28, r0, store, 4*28),
			269 => ins6556X(ext1, r29, r0, store, 4*29),
			270 => ins6556X(ext1, r30, r0, store, 4*30),
			271 => ins6556X(ext1, r31, r0, store, 4*31),
			272 => insRET,
			
			-- Fill registers (from address in r4)
			-- @1120
			280 => ins6556X(ext1, r0, r4, load, 4*0),
			281 => ins6556X(ext1, r1, r4, load, 4*1),
			282 => ins6556X(ext1, r2, r4, load, 4*2),
			283 => ins6556X(ext1, r3, r4, load, 4*3),
			284 => ins6556X(ext1, r0, r4, load, 4*4), -- CAREFUL! to r0, because input arg is in r4
			285 => ins6556X(ext1, r5, r4, load, 4*5),
			286 => ins6556X(ext1, r6, r4, load, 4*6),
			287 => ins6556X(ext1, r7, r4, load, 4*7),
			288 => ins6556X(ext1, r8, r4, load, 4*8),
			289 => ins6556X(ext1, r9, r4, load, 4*9),
			290 => ins6556X(ext1, r10, r4, load, 4*10),
			291 => ins6556X(ext1, r11, r4, load, 4*11),
			292 => ins6556X(ext1, r12, r4, load, 4*12),
			293 => ins6556X(ext1, r13, r4, load, 4*13),
			294 => ins6556X(ext1, r14, r4, load, 4*14),
			295 => ins6556X(ext1, r15, r4, load, 4*15),			
			296 => ins6556X(ext1, r16, r4, load, 4*16),
			297 => ins6556X(ext1, r17, r4, load, 4*17),
			298 => ins6556X(ext1, r18, r4, load, 4*18),
			299 => ins6556X(ext1, r19, r4, load, 4*19),
			300 => ins6556X(ext1, r20, r4, load, 4*20),
			301 => ins6556X(ext1, r21, r4, load, 4*21),
			302 => ins6556X(ext1, r22, r4, load, 4*22),
			303 => ins6556X(ext1, r23, r4, load, 4*23),
			304 => ins6556X(ext1, r24, r4, load, 4*24),
			305 => ins6556X(ext1, r25, r4, load, 4*25),
			306 => ins6556X(ext1, r26, r4, load, 4*26),
			307 => ins6556X(ext1, r27, r4, load, 4*27),
			308 => ins6556X(ext1, r28, r4, load, 4*28),
			309 => ins6556X(ext1, r29, r4, load, 4*29),
			310 => ins6556X(ext1, r30, r4, load, 4*30),
			311 => ins6556X(ext1, r0, r4, load, 4*31), -- CAREFUL: to r0, because r31 has return address
			312 => ins6556X(ext1, r4, r4, load, 4*4), -- Finally reading into r4			
			313 => insRET,
			
			-- Test result forwarding as src1
			-- @1280
			320 => --insNOP,
						insSET(r30, ERR_HANDLER),
			321 => ins655H(addI, r1, r0, 300),
			322 => ins655655(ext0, r2, r0, addR, r1, 0),
			323 => ins655655(ext0, r3, r0, addR, r1, 0),
			324 => ins655655(ext0, r4, r0, addR, r1, 0),
			325 => ins655655(ext0, r5, r0, addR, r1, 0),
			326 => ins655655(ext0, r6, r0, addR, r1, 0),
			327 => ins655655(ext0, r7, r0, addR, r1, 0),			
			-- Now check that it was passed everywhere
			328 => insMOVE(r10, r1),
			329 => ins655H(subI, r10, r10, 300), -- result must be 0
			330 => --ins65J(jnz, r10, 4*(1023 - 330)), -- if not, jump to illegal addr
						ins655655(ext1, r29, r10, jnzR, r30, 0),
			331 => insMOVE(r10, r2),
			332 => ins655H(subI, r10, r10, 300), -- result must be 0
			333 => ins65J(jnz, r10, 4*(1023 - 333)), -- if not, jump to illegal addr
			334 => insMOVE(r10, r3),
			335 => ins655H(subI, r10, r10, 300), -- result must be 0
			336 => ins65J(jnz, r10, 4*(1023 - 336)), -- if not, jump to illegal addr
			337 => insMOVE(r10, r4),
			338 => ins655H(subI, r10, r10, 300), -- result must be 0
			339 => ins65J(jnz, r10, 4*(1023 - 339)), -- if not, jump to illegal addr
			340 => insMOVE(r10, r5),
			341 => ins655H(subI, r10, r10, 300), -- result must be 0
			342 => ins65J(jnz, r10, 4*(1023 - 342)), -- if not, jump to illegal addr
			343 => insMOVE(r10, r6),
			344 => ins655H(subI, r10, r10, 300), -- result must be 0
			345 => ins65J(jnz, r10, 4*(1023 - 345)), -- if not, jump to illegal addr
			346 => insMOVE(r10, r7),
			347 => ins655H(subI, r10, r10, 300), -- result must be 0
			348 => ins65J(jnz, r10, 4*(1023 - 348)), -- if not, jump to illegal addr
			349 => insRET,
			
			-- Test forwarding as src0
			-- @1400
			350 => insNOP,
			351 => ins655H(addI, r11, r0, 400),
			352 => ins655655(ext0, r12, r11, addR, r0, 0),
			353 => ins655655(ext0, r13, r11, addR, r0, 0),
			354 => ins655655(ext0, r14, r11, addR, r0, 0),
			355 => ins655655(ext0, r15, r11, addR, r0, 0),
			356 => ins655655(ext0, r16, r11, addR, r0, 0),
			357 => ins655655(ext0, r17, r11, addR, r0, 0),
			-- Now check that it was passed everywhere
			358 => insMOVE(r10, r11),
			359 => ins655H(subI, r10, r10, 400), -- result must be 0
			360 => ins65J(jnz, r10, 4*(1023 - 360)), -- if not, jump to illegal addr
			361 => insMOVE(r10, r12),
			362 => ins655H(subI, r10, r10, 400), -- result must be 0
			363 => ins65J(jnz, r10, 4*(1023 - 363)), -- if not, jump to illegal addr
			364 => insMOVE(r10, r13),
			365 => ins655H(subI, r10, r10, 400), -- result must be 0
			366 => ins65J(jnz, r10, 4*(1023 - 366)), -- if not, jump to illegal addr
			367 => insMOVE(r10, r14),
			368 => ins655H(subI, r10, r10, 400), -- result must be 0
			369 => ins65J(jnz, r10, 4*(1023 - 369)), -- if not, jump to illegal addr
			370 => insMOVE(r10, r15),
			371 => ins655H(subI, r10, r10, 400), -- result must be 0
			372 => ins65J(jnz, r10, 4*(1023 - 372)), -- if not, jump to illegal addr
			373 => insMOVE(r10, r16),
			374 => ins655H(subI, r10, r10, 400), -- result must be 0
			375 => ins65J(jnz, r10, 4*(1023 - 375)), -- if not, jump to illegal addr
			376 => insMOVE(r10, r17),
			377 => ins655H(subI, r10, r10, 400), -- result must be 0
			378 => ins65J(jnz, r10, 4*(1023 - 378)), -- if not, jump to illegal addr
			379 => insRET,
			
			-- Test forwarding for 2 args
			-- @1520
			380 => insSET(r1,   91),
			381 => insSET(r2, 1002),
			382 => ins655655(ext0, r3, r1, addR, r2, 0),
			383 => ins655655(ext0, r4, r1, addR, r2, 0),
			384 => ins655655(ext0, r5, r4, addR, r1, 0), -- r4 + r1 = 2*r1 + r2 = 1184
			385 => ins655655(ext0, r6, r1, addR, r2, 0), 
			386 => ins655655(ext0, r7, r4, addR, r1, 0), -- r4 + r1 again
			387 => ins655655(ext0, r8, r1, addR, r2, 0),
			-- Now check
			388 => insMOVE(r10, r3),
			389 => ins655H(subI, r10, r10, 1093), -- result must be 0
			390 => ins65J(jnz, r10, 4*(1023 - 390)), -- if not, jump to illegal addr			
			391 => insMOVE(r10, r4),
			392 => ins655H(subI, r10, r10, 1093), -- result must be 0
			393 => ins65J(jnz, r10, 4*(1023 - 393)),
			394 => insMOVE(r10, r5),
			395 => ins655H(subI, r10, r10, 1184), -- result must be 0
			396 => ins65J(jnz, r10, 4*(1023 - 396)),
			397 => insMOVE(r10, r6),
			398 => ins655H(subI, r10, r10, 1093), -- result must be 0
			399 => ins65J(jnz, r10, 4*(1023 - 399)),			
			400 => insMOVE(r10, r7),
			401 => ins655H(subI, r10, r10, 1184), -- result must be 0
			402 => ins65J(jnz, r10, 4*(1023 - 402)), -- if not, jump to illegal addr			
			403 => insMOVE(r10, r8),
			404 => ins655H(subI, r10, r10, 1093), -- result must be 0
			405 => ins65J(jnz, r10, 4*(1023 - 405)),
			406 => insRET,
			
			-- Test store to load forwarding
			-- @1640
			410 => --insNOP,
					 ins655655(ext2, 0, 0, sync, 0, 0),
			411 => insCLEAR(r1),
			412 => insSET(r2, 22),
			413 => insSET(r3, 33),
			414 => insLOAD(r1, r0, 4*48), -- Should be 0
			415 => insSTORE(r2, r0, 4*49 - 4),
			416 => insLOAD(r5, r0, 4*49 - 4), -- Should be forwarded as 22
			417 => insSTORE(r3, r0, 4*50 - 8),
			418 => insLOAD(r6, r0, 4*50 - 8), -- Should be forwarded as 33
			419 => insSTORE(r1, r0, 4*48),
			420 => insLOAD(r7, r0, 4*48), -- Should be forwarded as 0
			
			421 => ins655H(subI, r5, r5, 22),
			422 => ins65J(jnz, r5, 4*(1023 - 422)),
			423 => ins655H(subI, r6, r6, 33),
			424 => ins65J(jnz, r6, 4*(1023 - 424)),
			425 => ins65J(jnz, r7, 4*(1023 - 425)),
			426 => insRET,
			
			-- Lower privilege to 0
			-- @1720
			430 => --insMOVE(r1, r0),
					--	insSET(r1, 1),
						insNOP,
			--431 => ins6556X(ext0, r1, r1, shlC, 25),
																-- Int: 3 link, 5 saved state
			431 => ins6556X(ext2, r0, 0, mtc, 5),	 -- lower the privilege
			432 => ins6556X(ext2, r31, 0, mtc, 3),	 -- write return address to ILR
			433 => ins655655(ext2, 0, 0, retI, 0, 0),
			
			-- Error handler
			-- @4000
			1000 => insNOP,
				-- r29 will hold return address
			1001 => --insNOP,
						insSTORE(r29, r0, 0), -- save r29 to address 0
			1002 => --insNOP,
						insERROR,
			1003 => ins655655(ext1, r0, r0, jzR, r29, 0), -- Return to r29 			
			
			
			others => insERROR -- undefined
		);
	
	
	constant testProgMem: WordMem := (
		0 => insNOP,
		1 => insSET(r1, 325),
		2 => insSET(r2, 0),
		3 => insNOP,--insSTORE(r1, r2, 12), -- This should go immediately
		4 => insLOAD(r5, r0, 12),

		

	
--		0 => insNOP,
--		1 => insSET(r1, 325),
--		2 => insSET(r2, 0),
--		3 => insSTORE(r1, r2, 12), -- This should go immediately
--		4 => insLOAD(r5, r0, 12), -- Addres hit, forwarding
--
--		5 => insNOP,
--				--insLOAD(r7, r0, 20), -- Addres hit, forwarding
--
--		6 => insNOP,
--		7 => --insNOP,
--				insSET(r5, 0),
--		8 => --insNOP,
--			  ins655655(ext0, r12, r5, mulS, r5, 0), -- long operation, delaying store address
--		9 => --insNOP,
--			  insSTORE(r2, r12, 16),
--		10 => --insNOP,
--				insLOAD(r10, r0, 16), -- younger load, address hit by older but later store
--		11 => insNOP,
--		12 => insNOP,
--		13 => insNOP,
--		14 => insNOP,
--		
--		15 => insSET(r2, 221),
--		16 => ins655655(ext0, r2, r5, mulS, r5, 0), -- long operation, delaying store address
--		17 => insNOP,--insSTORE(r2, r0, 16),
--		18 => insLOAD(r10, r0, 16), -- data not ready
--		
--		19 => insNOP,
--		20 => insNOP,
--		21 => insNOP,
--		22 => insNOP,
--		23 => insNOP,
--		24 => insNOP,
--		25 => insNOP,
--		26 => insNOP,
--		27 => insNOP,
		28 => insNOP,
		
		29 => ins65J(jz, r0, -4*(29-0)),
		
		
		others => --insERROR
						insNOP
	);
	
end ProgramCode;



package body ProgramCode is
function insCLEAR(reg: integer) return Word is
begin
    return ins655H(addI, reg, r0, 0);
end function;

function insSET(reg, num: integer) return Word is
begin
    return ins655H(addI, reg, r0, num);
end function;        

function insMOVE(rd, rs: integer) return Word is
begin
    return ins655H(orI, rd, rs, 0);
end function;

function insSTORE(ra, rb, num: integer) return Word is
begin
    return ins6556X(ext1, ra, rb, store, num);
end function;

function insLOAD(ra, rb, num: integer) return Word is
begin
    return ins6556X(ext1, ra, rb, load, num);
end function;

 
end ProgramCode;
