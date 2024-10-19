-- A skeleton of an ADA program for an assignment in programming languages

with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;

with Ada.Float_Text_IO;

procedure Simulation is

   ----GLOBAL VARIABLES---

   Number_Of_Producers       : constant Integer := 5;
   Number_Of_Assemblies      : constant Integer := 3;
   Number_Of_Consumers       : constant Integer := 2;
   Number_Of_Furious_Workers : constant Integer := 1;

   subtype Producer_Type is Integer range 1 .. Number_Of_Producers;
   subtype Assembly_Type is Integer range 1 .. Number_Of_Assemblies;
   subtype Consumer_Type is Integer range 1 .. Number_Of_Consumers;
   subtype Furious_Worker_Type is Integer range 1 .. Number_Of_Furious_Workers;

   --each Producer is assigned a Product that it produces
   Product_Name  : constant array (Producer_Type) of String (1 .. 8) :=
     ("Product1", "Product2", "Product3", "Product4", "Product5");
   --Assembly is a collection of products
   Assembly_Name : constant array (Assembly_Type) of String (1 .. 9) :=
     ("Assembly1", "Assembly2", "Assembly3");

   Furious_Worker_Name :
     constant array (Furious_Worker_Type) of String (1 .. 8) :=
     (1 => "Furious1");

   ----TASK DECLARATIONS----

   -- Producer produces determined product
   task type Producer is
      entry Start (Product : in Producer_Type; Production_Time : in Integer);
   end Producer;

   -- Consumer gets an arbitrary assembly of several products from the buffer
   -- but he/she orders it randomly
   task type Consumer is
      entry Start
        (Consumer_Number : in Consumer_Type; Consumption_Time : in Integer);
   end Consumer;

   -- Furious_Worker gets angry randomly and destroys products in buffer
   task type Furious_Worker is
      entry Start
        (Furious_Worker_Number : Furious_Worker_Type;
         Fury_Threshold_Arg    : Integer);
   end Furious_Worker;

   -- Buffer receives products from Producers and delivers Assemblies to Consumers
   task type Buffer is
      -- Accept a product to the storage (provided there is a room for it)
      entry Take (Product : in Producer_Type; Number : in Integer);
      -- Deliver an assembly (provided there are enough products for it)
      entry Deliver (Assembly : in Assembly_Type; Number : out Integer);
      -- Half the amount of every product type
      entry Quarrel_In_Storage (Worker_Number : Furious_Worker_Type);
   end Buffer;

   --Producer--
   P : array (1 .. Number_Of_Producers) of Producer;
   K : array (1 .. Number_Of_Consumers) of Consumer;
   W : array (1 .. Number_Of_Furious_Workers) of Furious_Worker;
   B : Buffer;

   ----TASK DEFINITIONS----

   task body Producer is
      subtype Production_Time_Range is Integer range 1 .. 3;
      package Random_Production is new Ada.Numerics.Discrete_Random
        (Production_Time_Range);
      --  random number generator
      G                    : Random_Production.Generator;
      Producer_Type_Number : Integer;
      Product_Number       : Integer;
      Production           : Integer;
      Random_Time          : Duration;

   begin
      accept Start (Product : in Producer_Type; Production_Time : in Integer)
      do
         --  start random number generator
         Random_Production.Reset (G);
         Product_Number       := 1;
         Producer_Type_Number := Product;
         Production           := Production_Time;
      end Start;
      Put_Line
        (ESC & "[93m" & "P: Started producer of " &
         Product_Name (Producer_Type_Number) & ESC & "[0m");
      loop
         Random_Time := Duration (Random_Production.Random (G));
         delay Random_Time;
         Put_Line
           (ESC & "[93m" & "P: Produced product " &
            Product_Name (Producer_Type_Number) & " number " &
            Integer'Image (Product_Number) & ESC & "[0m");
         -- Accept for storage
         B.Take (Producer_Type_Number, Product_Number);
         Product_Number := Product_Number + 1;
      end loop;
   end Producer;

   --Consumer--

   task body Consumer is
      subtype Consumption_Time_Range is Integer range 4 .. 8;
      package Random_Consumption is new Ada.Numerics.Discrete_Random
        (Consumption_Time_Range);

      --each Consumer takes any (random) Assembly from the Buffer
      package Random_Assembly is new Ada.Numerics.Discrete_Random
        (Assembly_Type);

      G               : Random_Consumption.Generator;
      GA              : Random_Assembly.Generator;
      Consumer_Nb     : Consumer_Type;
      Assembly_Number : Integer;
      Consumption     : Integer;
      Assembly_Type   : Integer;
      Consumer_Name   :
        constant array (1 .. Number_Of_Consumers) of String (1 .. 9) :=
        ("Consumer1", "Consumer2");

      procedure Consume_Assembly (Assembly_Type : Integer) is
      begin
         Assembly_Number := 0;
         B.Deliver (Assembly_Type, Assembly_Number);
         if Assembly_Number = 0 then
            Put_Line
              (ESC & "[96m" & "C: " & Consumer_Name (Consumer_Nb) &
               " didn't receive assembly " & Assembly_Name (Assembly_Type) &
               ESC & "[0m");
            delay 1.0;
         else
            Put_Line
              (ESC & "[96m" & "C: " & Consumer_Name (Consumer_Nb) &
               " takes assembly " & Assembly_Name (Assembly_Type) &
               " number " & Integer'Image (Assembly_Number) & ESC & "[0m");
         end if;
      end Consume_Assembly;

   begin
      accept Start
        (Consumer_Number : in Consumer_Type; Consumption_Time : in Integer)
      do
         Random_Consumption.Reset (G);
         Random_Assembly.Reset (GA);
         Consumer_Nb := Consumer_Number;
         Consumption := Consumption_Time;
      end Start;
      Put_Line
        (ESC & "[96m" & "C: Started consumer " & Consumer_Name (Consumer_Nb) &
         ESC & "[0m");
      loop
         delay Duration
           (Random_Consumption.Random (G)); --  simulate consumption
         Assembly_Type := Random_Assembly.Random (GA);
         -- take an assembly for consumption
         Consume_Assembly (Assembly_Type);
      end loop;
   end Consumer;

   --Furious_Worker--

   task body Furious_Worker is
      subtype Fury_Range is Integer range 1 .. 10;
      package Random_Fury_Level is new Ada.Numerics.Discrete_Random
        (Fury_Range);

      G : Random_Fury_Level.Generator;

      Fury_Level     : Integer;
      Worker_Number  : Integer;
      Fury_Threshold : Integer;
   begin
      accept Start
        (Furious_Worker_Number : Furious_Worker_Type;
         Fury_Threshold_Arg    : Integer)
      do
         Worker_Number  := Furious_Worker_Number;
         Fury_Threshold := Fury_Threshold_Arg;
      end Start;
      loop
         delay 5.0;
         Fury_Level := Random_Fury_Level.Random (G);
         if Fury_Level > Fury_Threshold then
            Put_Line
              (ESC & "[35m" & "W: Furious worker " &
               Furious_Worker_Name (Worker_Number) & " reached fury level " &
               Fury_Level'Image & ESC & "[0m");
            B.Quarrel_In_Storage (Worker_Number);
         end if;
      end loop;
   end Furious_Worker;

   --Buffer--

   task body Buffer is
      -- Consumer Stats
      Total_Assembly_Requests  : Float := 0.0;
      Total_Fulfilled_Requests : Float := 0.0;
      Total_Denied_Requests    : Float := 0.0;

      Storage_Capacity : constant Integer := 30;
      type Storage_type is array (Producer_Type) of Integer;
      Storage              : Storage_type := (0, 0, 0, 0, 0);
      Assembly_Content     : array (Assembly_Type, Producer_Type) of Integer :=
        ((2, 1, 2, 0, 2), (1, 2, 0, 1, 0), (3, 2, 2, 0, 1));
      Max_Assembly_Content : array (Producer_Type) of Integer;
      Assembly_Number      : array (Assembly_Type) of Integer := (1, 1, 1);
      In_Storage           : Integer := 0;

      -- Normalized rating of how in demand a certain product is (higher the number, higher the demand)
      Product_Demand : array (Producer_Type) of Float;

      procedure Set_Product_Demand is
         Sum_Producer_Demand : Integer := 0;
      begin
         for W in Producer_Type loop
            Product_Demand (W) := 0.0;
            for Z in Assembly_Type loop
               Product_Demand (W)  :=
                 Product_Demand (W) + Float (Assembly_Content (Z, W));
               Sum_Producer_Demand :=
                 Sum_Producer_Demand + Assembly_Content (Z, W);
            end loop;
         end loop;

         for W in Producer_Type loop
            Product_Demand (W) :=
              (Product_Demand (W) / Float (Sum_Producer_Demand));
         end loop;
      end Set_Product_Demand;

      procedure Setup_Variables is
      begin
         for W in Producer_Type loop
            Max_Assembly_Content (W) := 0;
            for Z in Assembly_Type loop
               if Assembly_Content (Z, W) > Max_Assembly_Content (W) then
                  Max_Assembly_Content (W) := Assembly_Content (Z, W);
               end if;
            end loop;
         end loop;
         Set_Product_Demand;
      end Setup_Variables;

      function Product_Storage_Weight (Product : Producer_Type) return Float is
         function Calc_Weight (x : Float) return Float is
            -- https://www.desmos.com/calculator/tynm88glpq

            -- Storage_Capacity casted to float
            cap   : constant Float := Float (Storage_Capacity);
            -- How much to shift the equation (used to shape the weight calculation curve)
            shift : constant Float := 0.1;

            function Weight_Equation (x : Float) return Float is
            begin
               return (cap - x - shift) / (x + shift);
            end Weight_Equation;

            -- Max and Min over function range (0 to Storage_Capacity)
            -- (used for normalization)
            max : constant Float := Weight_Equation (0.0);
            min : constant Float := Weight_Equation (cap);

            function Normalize (value : Float) return Float is
            begin
               return (value - min) / (max - min);
            end Normalize;

            Product_In_Storage : Float := Float (Storage (Product));
         begin
            return Normalize (Weight_Equation (Product_In_Storage));
         end Calc_Weight;

         InStorageWeight : Float;
      begin
         InStorageWeight := Calc_Weight (Float (Storage (Product)));
         return InStorageWeight * Product_Demand (Product);
      end Product_Storage_Weight;

      function Can_Accept_After_Remove
        (Product : Producer_Type; IndexToRemove : out Producer_Type)
         return Boolean
      is
         type Weights_Array_Type is array (Producer_Type) of Float;
         Products_Weights : Weights_Array_Type;

         function Select_For_Removal
           (Weights : Weights_Array_Type) return Producer_Type
         is
            function Should_Remove
              (P : Producer_Type; MinIndex : Producer_Type) return Boolean
            is
            begin
               -- Selects for the product with the following conditions
               --    - has the lowest possible weight
               --    - there is more of it in storage than 0
               --    - we have more of it than is required to fullfil any given assembly
               return
                 Weights (P) < Weights (MinIndex) and then Storage (P) > 0
                 and then Storage (P) > Max_Assembly_Content (P);
            end Should_Remove;

            MinIndex : Producer_Type;
         begin
            -- Find first removable item
            for P in Producer_Type loop
               -- Since this function gets called only when Storage is full we can be sure that at least one value is non zero
               if Storage (P) /= 0 then
                  MinIndex := P;
                  exit;
               end if;
            end loop;
            -- Find the least needed item
            for P in Producer_Type loop
               if Should_Remove (P, MinIndex) then
                  MinIndex := P;
               end if;
            end loop;
            return MinIndex;
         end Select_For_Removal;

      begin
         for W in Producer_Type loop
            Products_Weights (W) := Product_Storage_Weight (W);
         end loop;
         IndexToRemove := Select_For_Removal (Products_Weights);
         return (IndexToRemove /= Product);
      end Can_Accept_After_Remove;

      function Can_Accept (Product : Producer_Type) return Boolean is
         IndexToRemove : Producer_Type;
      begin
         if In_Storage < Storage_Capacity then
            return True;
         elsif Can_Accept_After_Remove (Product, IndexToRemove) then
            Storage (IndexToRemove) := Storage (IndexToRemove) - 1;
            In_Storage              := In_Storage - 1;
            Put_Line
              (ESC & "[91m" & "B: Removed product " &
               Product_Name (IndexToRemove) & " from storage " & ESC & "[0m");
            return True;
         else
            return False;
         end if;
      end Can_Accept;

      function Can_Deliver (Assembly : Assembly_Type) return Boolean is
      begin
         for W in Producer_Type loop
            if Storage (W) < Assembly_Content (Assembly, W) then
               return False;
            end if;
         end loop;
         return True;
      end Can_Deliver;

      procedure Consumer_Stats is
      begin
         Put ("|   Success: ");
         Ada.Float_Text_IO.Put
           (Item => Total_Fulfilled_Requests / Total_Assembly_Requests,
            Fore => 2, Aft => 2, Exp => 0);
         Put ("  Failure: ");
         Ada.Float_Text_IO.Put
           (Item => Total_Denied_Requests / Total_Assembly_Requests, Fore => 2,
            Aft  => 2, Exp => 0);
         Put_Line ("  Total:" & Integer (Total_Assembly_Requests)'Image);
      end Consumer_Stats;

      procedure Print_Weight (P : Producer_Type) is
      begin
         Put (" " & Product_Name (P) & " Weight: ");
         Ada.Float_Text_IO.Put
           (Item => Product_Storage_Weight (P), Fore => 1, Aft => 4, Exp => 0);
      end Print_Weight;

      procedure Storage_Contents is
      begin
         for P in Producer_Type loop
            Put ("|   Storage contents: ");
            Ada.Integer_Text_IO.Put (Storage (P), 2);
            --  Print_Weight (P);
            Put_Line ("");
         end loop;
         Put_Line
           ("|   Number of products in storage: " &
            Integer'Image (In_Storage));
         --  Consumer_Stats;
      end Storage_Contents;

      procedure Throwing_Products (Worker_Number : Furious_Worker_Type) is
      begin
         In_Storage := 0;
         for P in Producer_Type loop
            -- Integer division rounding down
            Storage (P) := (Storage (P) + 1) / 2;
            In_Storage  := In_Storage + Storage (P);
         end loop;
         Put_Line
           (ESC & "[91m" & "B: After " & Furious_Worker_Name (Worker_Number) &
            "'s fury, we lost half of the products in the storage" & ESC &
            "[0m");
         Storage_Contents;
      end Throwing_Products;

      procedure Handle_Take (Product : in Producer_Type; Number : in Integer)
      is
      begin
         if Can_Accept (Product) then
            Put_Line
              (ESC & "[91m" & "B: Accepted product " & Product_Name (Product) &
               " number " & Integer'Image (Number) & ESC & "[0m");
            Storage (Product) := Storage (Product) + 1;
            In_Storage        := In_Storage + 1;
         else
            Put_Line
              (ESC & "[91m" & "B: Rejected product " & Product_Name (Product) &
               " number " & Integer'Image (Number) & ESC & "[0m");
         end if;
         Storage_Contents;
      end Handle_Take;

      procedure Handle_Deliver
        (Assembly : in Assembly_Type; Number : out Integer)
      is
      begin
         Total_Assembly_Requests := Total_Assembly_Requests + 1.0;
         if Can_Deliver (Assembly) then
            Total_Fulfilled_Requests := Total_Fulfilled_Requests + 1.0;
            Put_Line
              (ESC & "[91m" & "B: Delivered assembly " &
               Assembly_Name (Assembly) & " number " &
               Integer'Image (Assembly_Number (Assembly)) & ESC & "[0m");
            for W in Producer_Type loop
               Storage (W) := Storage (W) - Assembly_Content (Assembly, W);
               In_Storage  := In_Storage - Assembly_Content (Assembly, W);
            end loop;
            Number                     := Assembly_Number (Assembly);
            Assembly_Number (Assembly) := Assembly_Number (Assembly) + 1;
         else
            Total_Denied_Requests := Total_Denied_Requests + 1.0;
            Put_Line
              (ESC & "[91m" & "B: Lacking products for assembly " &
               Assembly_Name (Assembly) & ESC & "[0m");
            Number := 0;
         end if;
         Storage_Contents;
      end Handle_Deliver;

   begin
      Put_Line (ESC & "[91m" & "B: Buffer started" & ESC & "[0m");
      Setup_Variables;
      loop
         -- Handle all priority tasks
         loop
            select
               accept Take (Product : in Producer_Type; Number : in Integer) do
                  Handle_Take (Product, Number);
               end Take;
            or
               accept Quarrel_In_Storage (Worker_Number : Furious_Worker_Type)
               do
                  Throwing_Products (Worker_Number);
               end Quarrel_In_Storage;
            else
               exit;
            end select;
         end loop;
         -- Handle one of any tasks
         select
            accept Deliver (Assembly : in Assembly_Type; Number : out Integer)
            do
               Handle_Deliver (Assembly, Number);
            end Deliver;
         or
            accept Take (Product : in Producer_Type; Number : in Integer) do
               Handle_Take (Product, Number);
            end Take;
         or
            accept Quarrel_In_Storage (Worker_Number : Furious_Worker_Type) do
               Throwing_Products (Worker_Number);
            end Quarrel_In_Storage;
         end select;
      end loop;
   end Buffer;

   ---"MAIN" FOR SIMULATION---
begin
   for I in 1 .. Number_Of_Producers loop
      P (I).Start (I, 10);
   end loop;
   for J in 1 .. Number_Of_Consumers loop
      K (J).Start (J, 12);
   end loop;
   for K in 1 .. Number_Of_Furious_Workers loop
      W (K).Start (K, 1);
   end loop;
end Simulation;
