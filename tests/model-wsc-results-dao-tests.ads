--
-- copyright(c) 2011 Graham Stark/ Virtual Worlds (graham.stark@virtual-worlds.biz)/ Howard Reed, Landman Economics (howard@landman-economics.co.uk)
--
-- ////////////////////////////////
--
-- This is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 3, or (at your option)
-- any later version.
-- 
-- It is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this software; see the file docs/gpl_v3.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street,
-- Boston, MA 02110-1301, USA.
-- 
-- /////////////////////////////

pragma License( Modified_GPL );

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with AUnit;
with AUnit.Test_Cases; 

--
-- Unit tests for household transition modelling, using AUnit V3.
-- See the AUnit Documentation
-- 
package Model.WSC.Results.DAO.Tests is
   
   use AUnit;
   use AUnit.Test_Cases;
   
   type Test_Case is new AUnit.Test_Cases.Test_Case with null record;
   
   procedure Register_Tests( tc : in out Test_Case );
   --  Register routines to be run
   
   function Name( tc : Test_Case ) return Message_String;
   --  Returns name identifying the test case
   
   --  Override if needed. Default empty implementations provided:
   
   --  Preparation performed before each routine:
   procedure Set_Up ( tc : in out Test_Case );


end Model.WSC.Results.DAO.Tests;
