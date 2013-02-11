--  * * * * * * * * * * * * * * * * * * * * * * * * * * * *
--  *                     U N I S E X                     *
--  *                                                     *
--  * A unisex bathroom queueing system, implemented      *
--  * concurrently using Ada's rendez-vous paradigm.      *
--  *                                                     *
--  * Author:     Riley Teige                             *
--  * Date:       May 27, 2012                            *
--  * Course:     CSCI 322                                *
--  * Instructor: Dr. Michael Meehan                      *
--  *                                                     *
--  * * * * * * * * * * * * * * * * * * * * * * * * * * * *

WITH Ada.Text_IO;                  USE Ada.Text_IO;
WITH Ada.Integer_Text_IO;          USE Ada.Integer_Text_IO;
WITH Ada.Command_Line;             USE Ada.Command_Line;
WITH Ada.Numerics.Discrete_Random;

PROCEDURE Unisex IS

	NUM_STALLS: Positive;
	SUBTYPE ID_Type IS Natural;
	TYPE Gender IS (Male, Female, Janitor);

--=============================================================================
	--OUTPUT
	--Guarantees exclusive access to the output buffer.
	
	PROTECTED Output IS
		PROCEDURE Print(Msg: String);
		PROCEDURE Print_Int(Int: Integer);
		PROCEDURE Print_Line(Msg: String);
		PROCEDURE Next_Line(N: Ada.Text_IO.Count := 1);
	END Output;

	PROTECTED BODY Output IS
		--*********************************************************************
		--Prints a line of text.
		PROCEDURE Print(Msg: String) IS
		BEGIN
			Ada.Text_IO.Put(Msg);
		END Print;
		--*********************************************************************

		PROCEDURE Print_Int(Int: Integer) IS
		BEGIN
			Ada.Integer_Text_IO.Put(Int, 6);
		END Print_Int;

		--*********************************************************************
		--Prints a line of text, then terminates the line.
		PROCEDURE Print_Line(Msg: String) IS
		BEGIN
			Ada.Text_IO.Put_Line(Msg);
		END Print_Line;
		--*********************************************************************
		
		--*********************************************************************
		--Prints a new-line character.
		PROCEDURE Next_Line(N: Ada.Text_IO.Count := 1) IS
		BEGIN
			Ada.Text_IO.New_Line(N);
		END Next_Line;
		--*********************************************************************
	END Output;
--=============================================================================

--=============================================================================
	--Returns a string of N spaces.
	FUNCTION PutSpaces(N: Natural) RETURN String IS
		S: String(1..N) := (OTHERS => ' ');
	BEGIN
		RETURN S;
	END PutSpaces;
--=============================================================================

--=============================================================================
	--Returns a uniquely offset string for each gender.
	FUNCTION GenStr(Sex: Gender) RETURN String IS
	BEGIN
		CASE (Sex) IS
		WHEN Male => Return PutSpaces(30)&Gender'Image(Male);
		WHEN Female => Return PutSpaces(70)&Gender'Image(Female);
		WHEN OTHERS => Return PutSpaces(110)&Gender'Image(Janitor);
		END CASE;
	END GenStr;
--=============================================================================

--=============================================================================
	--Returns the minimum of two numbers.
	FUNCTION Min(X, Y: Integer) RETURN Integer IS
	BEGIN
		IF X < Y THEN
			RETURN X;
		END IF;
		RETURN Y;
	END Min;
--=============================================================================

--=============================================================================
	--Defines the input thread.
	TASK Input IS
		ENTRY Start;
		ENTRY Finish;
		ENTRY DoTick;
		ENTRY RecvTick;
	END Input;
--=============================================================================
	
--=============================================================================
	--Each person is implemented as a separate thread.
	TYPE Person;
	TYPE Person_Ptr IS ACCESS Person;
	TASK TYPE Person(S: Gender; T: Natural; I: ID_Type) IS
		ENTRY GiveHandle(X: Person_Ptr);
		ENTRY ClockController(I: ID_Type);
		ENTRY ShowOut;
		ENTRY Kill;
	END Person;
--=============================================================================

--=============================================================================
	-- Maintains a logical time for the threads.
	PROTECTED Clock IS
		FUNCTION GetTime RETURN Natural;
		PROCEDURE Tick(N: Positive := 1);
	PRIVATE
		Time: Natural := 0;
	END Clock;

	PROTECTED BODY Clock IS
		-- Returns the current time.
		FUNCTION GetTime RETURN Natural IS
		BEGIN
			RETURN Time;
		END GetTime;

		-- Ticks the clock N times.
		PROCEDURE Tick(N: Positive := 1) IS
		BEGIN
			FOR I IN 1..N LOOP
				Time := Time + 1;
			END LOOP;
		END Tick;
	END Clock;
--=============================================================================	

--=============================================================================
	--A coin will be flipped to determine if a male will leave with the
	--seat of a toilet in the UP position or the DOWN position.
	TYPE Side IS (Heads, Tails);
	PROTECTED Coin IS
		FUNCTION Toss RETURN Side;
	END Coin;
	
	PROTECTED BODY Coin IS
		FUNCTION Toss RETURN Side IS
			PACKAGE Random_Coin IS NEW Ada.Numerics.Discrete_Random (Side);
			USE Random_Coin;
			G: Generator;
		BEGIN
			Reset(G);
			RETURN Random(G);
		END Toss;
	END Coin;
--=============================================================================

--*****************************************************************************
--=============================================================================
--*****************************************************************************
	--The bathroom is implemented as a server, connecting every thread into
	--a networked unit, implementing the major semantics of the solved problem.
	TASK Bathroom IS
		ENTRY Start;
		ENTRY Knock(Gender);
		ENTRY Let_In(Gender)(Handle: Person_Ptr; ID: ID_Type; How_Long: Integer);
		ENTRY Display;
		ENTRY Pause_Input(How_Long: Integer);
		ENTRY Unpause_Input;
		ENTRY Total_Created(X: Natural);
		ENTRY Finish;
	END Bathroom;

	TASK BODY Bathroom IS
		Current_Sex: Gender := Male;		-- Tracks current gender status.
		Switched: Boolean := False;
		
		Capacity: Positive;					-- Ensures we maintain capacity.
		Actual: Natural := 0;
		ToLetIn: Natural := 0;
		No_Males: Boolean := True;
		No_Females: Boolean := True;
		
		Num_Died: Natural := 0; 			-- Tracks number of threads killed.
		Total: Natural;						-- Final number of threads loaded.

		Janitor_Inside: Boolean := False;
		I: ID_Type;							-- Bookkeeping for new people.
		
		Min_ID: ID_Type := 0;				-- Used in assigning tick role.
		Min_Timer: Integer := Integer'Last;
		Input_Timer: Integer;
		Send_Input_Ticks: Boolean := False;
		
		Janitor_Handle: Person_Ptr := NULL;	-- Allows us to talk to Janitors.
		Kill_Handle: Person_Ptr := NULL;

		-- Here we are defining what a toilet looks like. Note the bookkeeping
		-- that will be done with this record, particularly the 'User' field,
		-- the crux of our rendez-vous capability.
		TYPE ToiletStatus IS (Up, Down);
		TYPE Stall IS RECORD
			ID: ID_Type;
			UID: ID_Type;
			Timer: Natural;
			Total: Natural;
			NotChanged: Natural;
			User: Person_Ptr;
			Status: ToiletStatus;
			Sex: Gender;
		END RECORD;

		-- This is where we define the DS that the people will be occupying.
		TYPE Stall_Array IS ARRAY(Positive Range <>) OF Stall;
		TYPE Stall_Array_Ptr IS ACCESS ALL Stall_Array;
		Toilets: Stall_Array_Ptr;

		--*********************************************************************
		-- Prints acknowledgment of entrance to STDOUT.
		PROCEDURE Entered(Sex: Gender; ID: ID_Type; Timer: Natural; How_Many: Integer) IS
		BEGIN
			Output.Print_Line(GenStr(Sex)&"#"&ID_Type'Image(ID)&
					" IN BATHROOM < < < TIMER="&Integer'Image(Timer)&", CLOCK="&Integer'Image(Clock.GetTime));
		END Entered;
		--*********************************************************************

		--*********************************************************************
		-- Prints acknowledgment of departure to STDOUT.
		PROCEDURE Leaving(Sex: Gender; ID: ID_Type) IS
		BEGIN
			Output.Print_Line(GenStr(Sex)&"#"&ID_Type'Image(ID)&
					" leaving the bathroom. CLOCK="&Integer'Image(Clock.GetTime));
		END Leaving;
		--*********************************************************************

		--*********************************************************************
		-- Changes the lid status of a toilet to UP or DOWN if the new
		-- state of the toilet would be different than the old state.
		PROCEDURE Put_Lid(Toilet: IN OUT Stall; New_Status: ToiletStatus) IS
		BEGIN
			IF Toilet.Status /= New_Status THEN
				--Output.Print_Line("Toilet #"&ID_Type'Image(Toilet.ID)&
				--		" lid put "&ToiletStatus'Image(New_Status));
				Toilet.Status := New_Status;
			END IF;
		END Put_Lid;
		--*********************************************************************

		--*********************************************************************
		-- Changes the current gender label for the bathroom.
		PROCEDURE Change_Sex(What: Gender) IS
		BEGIN
			Output.Print_Line("BATHROOM SWITCHED TO "&Gender'Image(What));
			Current_Sex := What;
		END Change_Sex;
		--*********************************************************************

		--*********************************************************************
		-- Determines occupancy of a stall. Improves code readability.
		FUNCTION Occupied(Toilet: Stall) RETURN BOOLEAN IS
		BEGIN
			RETURN Toilet.User /= NULL;
		END Occupied;
		--*********************************************************************

		--*********************************************************************
		-- Locates the first unoccupied stall starting from the first.
		-- Returns that stall's index into the stall array.
		FUNCTION Find_Unoccupied RETURN INTEGER IS
		BEGIN
			FOR I IN 1..Capacity LOOP
				IF NOT Occupied(Toilets(I)) THEN
					RETURN I;
				END IF;
			END LOOP;
			RETURN -1;
		END Find_Unoccupied;
		--*********************************************************************

		--*********************************************************************
		-- Allows a Person to occupy a stall in the bathroom. The person is
		-- then at the mercy of the clock.
		PROCEDURE Occupy(Dude: Person_Ptr; 
						 How_Long: Natural; 
						 ID: ID_Type; 
						 Sex: Gender; 
						 Toilet: IN OUT Stall) IS
		BEGIN
			IF Current_Sex = Male THEN
				IF Toilet.Status /= Up THEN
					Toilet.Total := Toilet.Total + 1;
				ELSE
					Toilet.NotChanged := Toilet.NotChanged + 1;
				END IF;
				Put_Lid(Toilet, Up);
			ELSIF Current_Sex = Female THEN
				IF Toilet.Status /= Down THEN
					Toilet.Total := Toilet.Total + 1;
				ELSE
					Toilet.NotChanged := Toilet.NotChanged + 1;
				END IF;
				Put_Lid(Toilet, Down);
			END IF;
			Toilet.User := Dude;
			Toilet.Timer := How_Long;
			Toilet.UID := ID;
			Toilet.Sex := Sex;
		END Occupy;
		--*********************************************************************

		--*********************************************************************
		-- Removes a person from their stall in the bathroom.
		PROCEDURE UnOccupy(Dude: Person_Ptr) IS
		BEGIN
			-- Locate person's stall
			FOR I IN 1..Capacity LOOP
				IF Toilets(I).User = Dude THEN

					-- Men flip a coin before leaving their stall.
					-- HEADS --> Seat stays up.
					-- TAILS --> Seat goes down.
					IF Toilets(I).Sex = Male THEN
						IF Coin.Toss = Heads THEN
							Put_Lid(Toilets(I), Up);
						ELSE
							Put_Lid(Toilets(I), Down);
						END IF;
					END IF;

					Toilets(I).User := NULL;
					Toilets(I).Timer := 0;
					Leaving(Toilets(I).Sex, Toilets(I).UID);
					Toilets(I).UID := 0;
					Actual := Actual - 1;
					EXIT;
				END IF;
			END LOOP;
		END UnOccupy;
		--*********************************************************************

		--*********************************************************************
		-- Resets bookkeeping stats for clock tick preparation.
		PROCEDURE Reset IS
		BEGIN
			Min_ID := 0;
			Min_Timer := INTEGER'Last;
		END Reset;
		--*********************************************************************

		--*********************************************************************
		-- Broadcasts a message to every thread attached to the bathroom,
		-- including the input thread if it is in a stalled state.
		PROCEDURE Broadcast IS
		BEGIN
			IF Actual = 0 AND Send_Input_Ticks AND Input_Timer > 0 THEN
				Input.DoTick;
				Input_Timer := 0;
			ELSE
				FOR N IN 1..Capacity LOOP
					IF Occupied(Toilets(N)) THEN
						Toilets(N).User.ClockController(Min_ID);
					END IF;
				END LOOP;
				IF Send_Input_Ticks AND Input_Timer > 0 THEN
					Input.RecvTick;
					Input_Timer := Input_Timer - 1;
				END IF;
			END IF;
		END Broadcast;
		--*********************************************************************

		--*********************************************************************
		-- Displays the status of every stall in the bathroom.
		-- Prints # of changes to toilet status, as well as the
		-- current status of the lid.
		PROCEDURE Print_Bathroom IS
		BEGIN
			Output.Next_Line(2);
			Output.Print_Line("BATHROOM STATUS");
			Output.Next_Line;
			Output.Print("Stall Number:  ");
			FOR I IN 1..Capacity LOOP
				Output.Print_Int(I);
			END LOOP;
			Output.Next_Line;
			Output.Print("Changed:       ");
			FOR I IN 1..Capacity LOOP
				Output.Print_Int(Toilets(I).Total);
			END LOOP;
			Output.Next_Line;
			Output.Print("Not changed:   ");
			FOR I IN 1..Capacity LOOP
				Output.Print_Int(Toilets(I).NotChanged);
			END LOOP;
			Output.Next_Line;
			Output.Print("Times occupied:");
			FOR I IN 1..Capacity LOOP
				Output.Print_Int(Toilets(I).Total + Toilets(I).NotChanged);
			END LOOP;
			Output.Next_Line;
			Output.Print("Final status:  ");
			FOR I IN 1..Capacity LOOP
				CASE Toilets(I).Status IS
					WHEN UP => Output.Print("     U");
					WHEN DOWN => Output.Print("     D");
				END CASE;
			END LOOP;
			Output.Next_Line(2);
		END Print_Bathroom;
		--*********************************************************************

	BEGIN
		-- Hold for the command line processing to finish.
		ACCEPT Start;
		
		Capacity := NUM_STALLS;
		Toilets := NEW Stall_Array(1..Capacity);
		
		-- Initialize all stats.
		FOR N IN 1..Capacity LOOP
			Toilets(N).ID     		:= ID_Type(N);
			Toilets(N).UID    		:= 0;
			Toilets(N).Total  		:= 0;
			Toilets(N).NotChanged 	:= 0;
			Toilets(N).Timer  		:= 0;
			Toilets(N).User   		:= NULL;
			Toilets(N).Status 		:= Down;
		END LOOP;

		-- Here is where the bathroom is listening and responding to other
		-- threads. This is really where the bulk of processing comes in.
		LOOP
			SELECT
				-- ******************** JANITOR GUARD ********************
				-- Janitor has top priority; they can enter when
				-- the bathroom is empty.
				WHEN Actual = 0 =>
					ACCEPT Knock(Janitor);

				-- The Janitor is past the guard, now we process them.
				ACCEPT Let_In(Janitor)(	Handle: Person_Ptr;
									 	ID: ID_Type;
										How_Long: Integer ) DO
					IF Janitor_Handle /= NULL THEN
						Kill_Handle := Handle;
					ELSE
						I := ID;
						Janitor_Inside := True;
						Janitor_Handle := Handle;
						Min_Timer := How_Long;
					END IF;
				END Let_In;				

				-- If another Janitor was in the bathroom, this one must go.
				IF Kill_Handle /= NULL THEN
					Num_Died := Num_Died + 1;
					Kill_Handle.Kill;
					Kill_Handle := NULL;
				ELSE
					Entered(Janitor, I, Min_Timer, 0);
					Min_ID := I;
				END IF;
			OR
				-- ******************** MALE GUARD ********************
				-- Males and Females have equal priority, but we must be
				-- sure not to allow for deadlock. First, make sure no Janitors
				-- are in line. Then, if there isn't one inside, make sure
				-- there is room in the bathroom. Then we verify the bathroom
				-- is our gender; if it isn't we check to see if it can become
				-- our gender. Then we check that there aren't any opposites in
				-- the bathroom (as they could have been about to leave and
				-- changed the gender to ours if they saw us in line).

				WHEN Knock(Janitor)'Count = 0 AND
					 NOT Janitor_Inside AND
					 Actual < Capacity AND
					 Current_Sex = Male AND-- (No_Females AND Knock(Female)'Count = 0)) AND
					 No_Females =>
					ACCEPT Knock(Male);
				
				-- Let everyone know there are men inside.
				No_Males := False;

				-- Either let in all the men or just those that can fit.
				ToLetIn := Min(Knock(Male)'Count+1, Capacity-Actual);
				FOR N IN 1..ToLetIn - 1 LOOP
					ACCEPT Knock(Male);
				END LOOP;

				-- Process them into a stall.
				FOR N IN 1..ToLetIn LOOP
					ACCEPT Let_In(Male)(Handle: Person_Ptr;
										ID: ID_Type;
										How_Long: Integer) DO
						I := ID;
						--IF Current_Sex = Female THEN
						--	Change_Sex(Male);
						--END IF;
						Actual := Actual + 1;
						Entered(Male, I, How_Long, Knock(Female)'Count);
						Occupy(Handle, How_Long, ID, Male, Toilets(Find_Unoccupied));
					END Let_In;
				END LOOP;
			OR
				-- ******************** FEMALE GUARD ********************
				-- Same guard as above.
				WHEN Knock(Janitor)'Count = 0 AND
					 NOT Janitor_Inside AND
					 Actual < Capacity AND
					 Current_Sex = Female AND --OR (No_Males AND Knock(Male)'Count = 0)) AND
					 No_Males =>
					ACCEPT Knock(Female);

				-- Let everyone know there are men inside.
				No_Females := False;

				-- Either let in all the women or just those that can fit.
				ToLetIn := Min(Knock(Female)'Count+1, Capacity-Actual);
				FOR N IN 1..ToLetIn - 1 LOOP
					ACCEPT Knock(Female);
				END LOOP;

				-- Process them into a stall.
				FOR N IN 1..ToLetIn LOOP
					ACCEPT Let_In(Female)(	Handle: Person_Ptr;
											ID: ID_Type;
											How_Long: Integer) DO
						I := ID;
						--IF Current_Sex = Male THEN
						--	Change_Sex(Female);
						--END IF;
						Actual := Actual + 1;
						Entered(Female, I, How_Long, Knock(Male)'Count);
						Occupy(Handle, How_Long, ID, Female, Toilets(Find_Unoccupied));
					END Let_In;
				END LOOP;
			OR
				-- Somebody wants the bathroom to print its status.
				ACCEPT Display DO
					Print_Bathroom;
				END Display;
			OR
				-- Input thread is stalled.
				ACCEPT Pause_Input(How_Long: Integer) DO
					Input_Timer := How_Long;
					Send_Input_Ticks := True;
				END Pause_Input;
			OR
				-- Input thread is reading again.
				ACCEPT Unpause_Input DO
					Input_Timer := 0;
					Send_Input_Ticks := False;
				END Unpause_Input;
			OR
				-- Input thread is done reading input.
				ACCEPT Total_Created(X: Natural) DO
					Total := X;
				END Total_Created;
			OR
				-- Input thread is waiting for kill signal.
				WHEN Total = Num_Died =>
				ACCEPT Finish;

			ELSE -- Nobody was let in.
				IF Janitor_Inside THEN -- Tick janitor.

					-- Tell the janitor to tick the clock.
					Janitor_Handle.ClockController(Min_ID);
				
					-- Decrement our janitor timer. Kick him
					-- out if necessary.
					Min_Timer := Min_Timer - 1;
					IF Min_Timer = 0 THEN
						Num_Died := Num_Died + 1;
						Janitor_Handle.ShowOut;
						Janitor_Inside := False;
						Janitor_Handle := NULL;
						Leaving(Janitor, I);
					END IF;

				ELSIF Actual /= 0 THEN-- Tick users.

					Reset;

					-- Find the user with the shortest remaining time.
					FOR N IN 1..Capacity LOOP
						IF Occupied(Toilets(N)) AND THEN Toilets(N).Timer < Min_Timer THEN
							Min_ID := Toilets(N).UID;
							Min_Timer := Toilets(N).Timer;
						END IF;
					END LOOP;

					-- Tell everyone who the clock controller is for this tick.
					Broadcast;

					-- Kick out anyone whose time is up.
					FOR N IN 1..Capacity LOOP
						IF Occupied(Toilets(N)) THEN
							 Toilets(N).Timer := Toilets(N).Timer - 1;
							 IF Toilets(N).Timer = 0 THEN
							 	Num_Died := Num_Died + 1;
							 	Toilets(N).User.ShowOut;
							 	UnOccupy(Toilets(N).User);
							 END IF;
						END IF;
					END LOOP;

					-- Change gender if necessary.
					IF Current_Sex = Male AND Knock(Female)'Count /= 0 AND NOT Switched THEN
						Switched := True;
						Change_Sex(Female);
					ELSIF Current_Sex = Female AND Knock(Male)'Count /= 0 AND NOT Switched THEN
						Switched := True;
						Change_Sex(Male);
					END IF;
				ELSE
					Broadcast;
				END IF;
			END SELECT;

			-- Post empty bathroom.
			IF Actual = 0 THEN
				Switched := False;
				No_Males := True;
				No_Females := True;
			END IF;
			
		END LOOP;
	END Bathroom;

--*****************************************************************************
--=============================================================================
--*****************************************************************************

--=============================================================================
	-- Implement the people who will be interacting with each other and the
	-- bathroom. A person needs to know who they are, how others can
	-- communicate with them, and when they're supposed to quit.
	TASK BODY Person IS
		Handle: Person_Ptr;				-- This is how we communicate.
		Sex: Gender := S;				-- This person's gender.
		Timer: Integer := T;			-- How many ticks they can sustain.
		ID: ID_Type := I;				-- Unique identifier.
		Exit_Thread: Boolean := False;	-- True when we are done in bathroom.
	BEGIN
		-- Get our communicating tool.
		ACCEPT GiveHandle(X: Person_Ptr) DO
			Handle := X;
		END GiveHandle;

		-- Try to enter the bathroom.
		Output.Print_Line(GenStr(Sex)&"#"&Integer'Image(ID)&" waiting... CLOCK ="&Integer'Image(Clock.GetTime));
		Bathroom.Knock(Sex);
		Bathroom.Let_In(Sex)(Handle, ID, Timer);

		-- Now we are in the bathroom. Either we do a tick, or we are
		-- told to leave.
		WHILE NOT Exit_Thread LOOP
			SELECT
				ACCEPT ClockController(I: ID_Type) DO
					IF I = ID THEN
						Clock.Tick;
					END IF;
				END ClockController;
			OR
				ACCEPT ShowOut DO
					Exit_Thread := True;
				END ShowOut;
			OR
				ACCEPT Kill DO
					Exit_Thread := True;
					Output.Print_Line(GenStr(Janitor)&"#"&
							ID_Type'Image(ID)&" was fired. CLOCK ="&Integer'Image(Clock.GetTime));
				END Kill;
			END SELECT;
		END LOOP;
		
	END Person;
--=============================================================================

--=============================================================================
	-- This is the input thread. Here we read from STDIN, generating "people"
	-- to occupy the bathroom in an order relative to their individual
	-- execution rate.
	TASK BODY Input IS
		Next_Sex: Character;		-- Stats for the next thread.
		Next_Gender: Gender;
		Next_Timer: Natural;
		Next_ID: ID_Type;

		Num_Men: Natural := 0;		-- Tracks the number of threads made
		Num_Women: Natural := 0;	-- of each gender. This will determine ID#
		Num_Janitors: Natural := 0;	-- of threads generated later.
		Num_Total: Natural := 0;
	
		X: Person_Ptr;
	BEGIN
		ACCEPT Start;
	
		WHILE NOT End_Of_File LOOP
			Get(Next_Sex);
			Get(Next_Timer);
			CASE (Next_Sex) IS

			WHEN 'M' =>	--Generate Male Thread
				Next_Gender := Male;
				Num_Men := Num_Men + 1;
				Next_ID := Num_Men;
				Output.Print_Line("Male #"&Integer'Image(Next_ID)&" being generated... CLOCK="&Integer'Image(Clock.GetTime));
				X := NEW Person(Male, Next_Timer, Next_ID);
				X.GiveHandle(X);
				Num_Total := Num_Total + 1;

			WHEN 'W' => -- Generate Female Thread
				Next_Gender := Female;
				Num_Women := Num_Women + 1;
				Next_ID := Num_Women;
				Output.Print_Line("Female #"&Integer'Image(Next_ID)&" being generated... CLOCK="&Integer'Image(Clock.GetTime));
				X := NEW Person(Female, Next_Timer, Next_ID);
				X.GiveHandle(X);
				Num_Total := Num_Total + 1;

			WHEN 'J' => -- Generate Janitor Thread
				Next_Gender := Janitor;
				Num_Janitors := Num_Janitors + 1;
				Next_ID := Num_Janitors;
				Output.Print_Line("Janitor #"&Integer'Image(Next_ID)&" being generated... CLOCK="&Integer'Image(Clock.GetTime));
				X := NEW Person(Janitor, Next_Timer, Next_ID);
				X.GiveHandle(X);
				Num_Total := Num_Total + 1;

			WHEN 'S' => -- Hold input for Next_Timer ticks

				-- Tell bathroom we are pausing.
				Output.Print_Line("Pausing input for"&Integer'Image(Next_Timer)&" ticks... CLOCK="&Integer'Image(Clock.GetTime));
				Bathroom.Pause_Input(Next_Timer);
				WHILE Next_Timer > 0 LOOP
					SELECT
						ACCEPT DoTick DO
							Output.Print_Line("Input thread is sending"&Integer'Image(Next_Timer)&" ticks. CLOCK="&Integer'Image(Clock.GetTime));
							Clock.Tick(Next_Timer);
							Next_Timer := 0;	-- Prevents busy waiting.
						END DoTick;
					OR
						ACCEPT RecvTick DO
							Next_Timer := Next_Timer - 1;
						END RecvTick;
					END SELECT;
				END LOOP;
				Output.Print_Line("Unpausing input... CLOCK="&Integer'Image(Clock.GetTime));
				Bathroom.Unpause_Input;	-- Tell bathroom we are resuming.

			WHEN OTHERS =>
				RAISE Constraint_Error;
			END CASE;
		END LOOP;

		Bathroom.Total_Created(Num_Total);
		Bathroom.Finish;

		ACCEPT Finish;
	END Input;
--=============================================================================

	Argument_Count_Error: EXCEPTION;

	PROCEDURE Command_Line IS
	BEGIN
		IF Argument_Count /= 1 THEN
			RAISE Argument_Count_Error;
		END IF;
		NUM_STALLS := Positive'Value(Argument(1));
		EXCEPTION WHEN Constraint_Error | Data_Error =>
			Put_Line("Bad arg: "&Argument(1));
			RAISE Argument_Count_Error;
	END Command_Line;

BEGIN

	
	Output.Next_Line(2);
	Output.Print_Line("********** PROGRAM START **********");
	Output.Next_Line(2);

	Command_Line;
	Bathroom.Start;
	Input.Start;
	
	Input.Finish;
	Bathroom.Display;
	Output.Print_Line("Final time is:"&Integer'Image(Clock.GetTime));
	Output.Next_Line;

	Output.Next_Line(2);
	Output.Print_Line("********** PROGRAM END **********");
	Output.Next_Line(2);

	ABORT Bathroom;
	ABORT Input;
	EXCEPTION WHEN Argument_Count_Error =>
		ABORT Bathroom;
		ABORT Input;
		Put_Line("Usage: unisex #stalls");
END Unisex;
