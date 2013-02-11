-- Author: David Frantz
-- Date: May 2012

-- ./make_input_data <number of instructions> <file name to make>

WITH ada.text_io;
USE  ada.text_io;
WITH ada.integer_text_io;
USE  ada.integer_text_io;
WITH ada.command_line;
USE  ada.command_line;
WITH ada.numerics.discrete_random;

PROCEDURE mid IS

TYPE coin IS (heads, tails);
PACKAGE random_coin IS NEW ada.numerics.discrete_random(coin);
USE random_coin;

SUBTYPE randRange IS integer RANGE 1..500;
PACKAGE random_int IS NEW ada.numerics.discrete_random(randRange);
USE random_int;

SUBTYPE units IS integer RANGE 2..15;
PACKAGE random_unit IS NEW ada.numerics.discrete_random(units);
USE random_unit;

C : random_coin.generator;
I : random_int.generator;
U : random_unit.generator;

argument_error : EXCEPTION;
naming_error : EXCEPTION;
OutData : file_type;

val : integer;

BEGIN
	IF (argument_count < 2) THEN
		RAISE argument_error;
	END IF;

	IF (argument(2) = "unisex.adb" OR argument(2) = "bathroom.adb") THEN
		RAISE naming_error;
	END IF;

	reset(C);
	reset(I);
	reset(U);

	Create(OutData, Out_File, argument(2));
	Set_output(outdata);

	FOR k IN 1..Integer'Value(argument(1)) LOOP
		val := random(I);
		IF (val > 490) THEN
			put("J "); put(random(U), 1); new_line;
		ELSIF (val < 10) THEN
			put("S "); put(random(U) + 5, 1); new_line;
		ELSE
			IF (random(C) = heads) THEN
				put("M "); put(random(U), 1); new_line;
			ELSE
				put("W "); put(random(U), 1); new_line;
			END IF;
		END IF;
	END LOOP;
	Close(OutData);

EXCEPTION
	WHEN argument_error =>
		put_line("Insufficient arguments: <number of instructions>, <file name to create>");
	WHEN naming_error =>
		put_line("You have chosen a name too closely associated with important files. Action aborted.");
END mid;
