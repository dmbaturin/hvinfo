------------------------------------------------------------------------
-- Copyright (C) 2015 Daniil Baturin <daniil@baturin.org>
--
-- This file is part of hvinfo.
--
-- hvinfo is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 2 of the License, or
-- (at your option) any later version.
--
-- hvinfo is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with hvinfo.  If not, see <http://www.gnu.org/licenses/>.
------------------------------------------------------------------------

with Ada.Command_line;
with Ada.Text_IO;

with Config;

package HVInfo_Util is

    package IO renames Ada.Text_IO;

    procedure Print_Version;
    procedure Print_Help;

end HVINfo_Util;
