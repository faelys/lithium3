------------------------------------------------------------------------------
-- Copyright (c) 2015-2019, Natacha Porté                                   --
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

with Ada.Real_Time;
with GNAT.SHA1;
with Natools.S_Expressions;
with Natools.Web.ACL.Sx_Backends;
with Natools.Web.Backends.Filesystem;
with Natools.Web.Comment_Cookies;
with Natools.Web.Cookie_Setters;
with Natools.Web.Escapes.Filters;
with Natools.Web.Exchanges;
with Natools.Web.Filters.Pass_Through;
with Natools.Web.Filters.Text_Blocks;
with Natools.Web.Filters.Text_Replacement;
with Natools.Web.Reload_Pages;
with Natools.Web.Simple_Pages.Dynamic_Multipages;
with Natools.Web.Simple_Pages.Markdown_Pages;
with Natools.Web.Simple_Pages.Markdown_Multipages;
with Natools.Web.Sites.Updates;
with Natools.Web.Tag_Pages;

with Lithium.Access_Log;
with Lithium.Comment_Cookie_Smaz;
with Lithium.Exception_Log;
with Lithium.Legacy_Filters;
with Lithium.Markdown.Filters;
with Lithium.Photo_Posts;
with Lithium.Spoiler_Filters;

package body Lithium.Dispatchers is

   function SHA1_Digest (Msg : in Natools.S_Expressions.Atom)
     return Natools.S_Expressions.Atom
     is (GNAT.SHA1.Digest (Msg));


   overriding function Clone (Object : Handler) return Handler is
   begin
      return Object;
   end Clone;


   overriding function Dispatch
     (Dispatcher : Handler;
      Request : AWS.Status.Data)
     return AWS.Response.Data
   is
      Aliased_Req : aliased constant AWS.Status.Data := Request;
      Exchange : aliased Natools.Web.Exchanges.Exchange (Aliased_Req'Access);
      Result : AWS.Response.Data;
      Start, Middle : Ada.Real_Time.Time;
   begin
      Start := Ada.Real_Time.Clock;
      Dispatcher.Ref.Update.Respond (Exchange);
      Middle := Ada.Real_Time.Clock;
      Result := Natools.Web.Exchanges.Response (Exchange);

      Access_Log.Log
        (Request, Result,
         Ada.Real_Time.To_Duration (Ada.Real_Time."-" (Middle, Start)),
         Ada.Real_Time.To_Duration (Ada.Real_Time."-"
           (Ada.Real_Time.Clock, Middle)));
      return Result;
   exception
      when Ex : others =>
         Exception_Log.Report (Ex, Request);
         return Exception_Log.Respond (Ex, Request);
   end Dispatch;


   not overriding function Create (File_Name : String) return Handler is
      Holder : constant Holder_Refs.Data_Access
        := new Natools.Web.Sites.Holders.Holder;
      Result : constant Handler
        := (AWS.Dispatchers.Handler with Ref => Holder_Refs.Create (Holder));
   begin
      Natools.Web.Simple_Pages.Dynamic_Multipages.Register
        ("photo-posts", Lithium.Photo_Posts.Create'Access);

      Natools.Web.ACL.Sx_Backends.Register ('1', SHA1_Digest'Access);
      Holder.Register
        ("s-expr", Natools.Web.ACL.Sx_Backends.Create'Access);

      Holder.Register
        ("cookie-setter",
         Natools.Web.Cookie_Setters.Create'Access);
      Holder.Register
        ("markdown-page",
         Natools.Web.Simple_Pages.Markdown_Pages.Create'Access);
      Holder.Register
        ("markdown-multipage",
         Natools.Web.Simple_Pages.Markdown_Multipages.Create'Access);
      Holder.Register
        ("multipage",
         Natools.Web.Simple_Pages.Dynamic_Multipages.Create'Access);
      Holder.Register
        ("reload-page", Natools.Web.Reload_Pages.Create'Access);
      Holder.Register
        ("simple-page", Natools.Web.Simple_Pages.Create'Access);
      Holder.Register
        ("tag-page", Natools.Web.Tag_Pages.Create'Access);

      Holder.Register
        ("directory", Natools.Web.Backends.Filesystem.Create'Access);

      Holder.Register
        ("comment-markdown", Lithium.Markdown.Filters.Create_Comment'Access);
      Holder.Register
        ("extended-markdown", Lithium.Markdown.Filters.Create_Extended'Access);
      Holder.Register
        ("html-escape", Natools.Web.Escapes.Filters.Create'Access);
      Holder.Register
        ("legacy-comment", Lithium.Legacy_Filters.Create'Access);
      Holder.Register
        ("pass-through", Natools.Web.Filters.Pass_Through.Create'Access);
      Holder.Register
        ("replace-text", Natools.Web.Filters.Text_Replacement.Create'Access);
      Holder.Register
        ("spoiler-filter", Lithium.Spoiler_Filters.Create'Access);
      Holder.Register
        ("text-block", Natools.Web.Filters.Text_Blocks.Create'Access);

      Holder.Register
        (Comment_Cookie_Smaz.Key, Comment_Cookie_Smaz.Decoder'Access);
      Holder.Set_Cookie_Encoder
        (Comment_Cookie_Smaz.Encoder'Access,
         Natools.Web.Comment_Cookies.Positional);

      Holder.Load (File_Name);

      return Result;
   end Create;


   not overriding procedure Purge (Object : in Handler) is
      Update : Natools.Web.Sites.Updates.Expiration_Purger;
   begin
      Object.Ref.Update.Queue (Update);
   end Purge;

end Lithium.Dispatchers;
