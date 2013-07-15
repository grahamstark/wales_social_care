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

with Logger;

with GNATCOLL.Traces;

--
-- This is a very, very basic logging package. It's different from your typical logger (e.g. JLog) in that
-- you activate it by package rather than by severity level. Possibly not threat safe.
--
package WSC_Logger is

   type Loggable_Modules is ( 
      queueing, 
      weights,
      transitions, 
      static_calcs,
      dynamic_calcs,
      callbacks, 
      data_creation, 
      household, 
      parameter_handlers, 
      users, 
      input_rendering, 
      output,
      dao,
      database,
      web_io );

	package E_Logger is new Logger( Loggable_Modules );
	
	procedure Log( which : Loggable_Modules; s : String ) renames E_Logger.Log;
	procedure Add_Target( which : Loggable_Modules ) renames E_Logger.Add_Target;
	procedure Flush renames E_Logger.Flush;
	procedure Set_Output( name : String ) renames E_Logger.Set_Output;
	procedure Add_All_Targets renames E_Logger.Add_All_Targets;
	procedure Remove_Target( which : Loggable_Modules ) renames E_Logger.Remove_Target;
	procedure Clear_All_Targets renames E_Logger.Clear_All_Targets;
	
end WSC_Logger;
