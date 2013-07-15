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
--
-- This is the top-level package for the Wales model. It does very little: just
-- imports some standard types and generic formatting and calculation routines, so these can  
-- be used easily in the model itself, which is (almost) all declared in child packages.
-- 
-- Putting anything else in here may cause nasty cross-dependencies.
--
--  $Author: graham_s $
--  $Date: 2008-11-01 21:53:28 +0000 (Sat, 01 Nov 2008) $
--  $Revision: 6124 $

pragma License( Modified_GPL );

with List_Of_Randoms;

package Model.WSC is

      package M_Randoms is new List_Of_Randoms(
      Real => Rate,
      Capacity => 20_000 );
   
end Model.WSC;
