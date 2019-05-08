------------------------------------------------------------------------------
-- Copyright (c) 2019, Natacha Porté                                        --
--                                                                          --
-- Permission to use, copy, modify, and distribute this software for any    --
-- purpose with or without fee is hereby granted, provided that the above   --
-- copyright notice and this permission notice appear in all copies.        --
--                                                                          --
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES --
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF         --
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR  --
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES   --
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN    --
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF  --
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.           --
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Lithium.Photo_Posts provides a Simple_Page generator from a POSTed form, --
-- for use with dynamic multipages.                                         --
------------------------------------------------------------------------------

with Natools.S_Expressions;
with Natools.S_Expressions.Lockable;
with Natools.Web.Simple_Pages.Dynamic_Multipages;
with Natools.Web.Sites;

private with Ada.Containers.Doubly_Linked_Lists;
private with Natools.S_Expressions.Atom_Refs;
private with Natools.S_Expressions.Caches;
private with Natools.Web.Containers;
private with Lithium.Photo_Process;

package Lithium.Photo_Posts is

   package MP renames Natools.Web.Simple_Pages.Dynamic_Multipages;
   package Sx renames Natools.S_Expressions;

   type Generator is new MP.Generator with private;

   overriding procedure Respond
     (Object : in out Generator;
      Exchange : in out Natools.Web.Sites.Exchange;
      Extra_Path : in Sx.Atom;
      Descriptor : out MP.Page_Descriptor;
      Do_Post : out Boolean);

   function Create
     (Arguments : in out Sx.Lockable.Descriptor'Class)
     return MP.Generator'Class;

private

   package Process_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Lithium.Photo_Process.Process_Description,
      Lithium.Photo_Process."=");

   type Generator is new MP.Generator with record
      Success_Redirect : Sx.Atom_Refs.Immutable_Reference;
      Failure_Redirect : Sx.Atom_Refs.Immutable_Reference;
      Tag_Prefix : Sx.Atom_Refs.Immutable_Reference;
      Name_Prefix : Sx.Atom_Refs.Immutable_Reference;
      Extra_Tags : Natools.Web.Containers.Atom_Array_Refs.Immutable_Reference;
      Yearly_Tags : Natools.Web.Containers.Atom_Array_Refs.Immutable_Reference;
      Monthly_Tags :
        Natools.Web.Containers.Atom_Array_Refs.Immutable_Reference;
      Process : Process_Lists.List;
      Allow : Sx.Caches.Reference;
   end record;

end Lithium.Photo_Posts;
