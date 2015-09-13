------------------------------------------------------------------------------
-- Copyright (c) 2015, Natacha Porté                                        --
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

with Natools.Web.Backends.Filesystem;
with Natools.Web.Escapes.Filters;
with Natools.Web.Exchanges;
with Natools.Web.Filters.Pass_Through;
with Natools.Web.Filters.Text_Blocks;
with Natools.Web.Simple_Pages.Markdown_Pages;
with Natools.Web.Tag_Pages;

with Lithium.Legacy_Filters;

package body Lithium.Dispatchers is

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
   begin
      Dispatcher.Ref.Update.Respond (Exchange);
      return Natools.Web.Exchanges.Response (Exchange);
   end Dispatch;


   not overriding function Create (File_Name : String) return Handler is
      Holder : constant Holder_Refs.Data_Access
        := new Natools.Web.Sites.Holders.Holder;
      Result : constant Handler
        := (AWS.Dispatchers.Handler with Ref => Holder_Refs.Create (Holder));
   begin
      Holder.Register
        ("markdown-page",
         Natools.Web.Simple_Pages.Markdown_Pages.Create'Access);
      Holder.Register
        ("simple-page", Natools.Web.Simple_Pages.Create'Access);
      Holder.Register
        ("tag-page", Natools.Web.Tag_Pages.Create'Access);

      Holder.Register
        ("directory", Natools.Web.Backends.Filesystem.Create'Access);

      Holder.Register
        ("html-escape", Natools.Web.Escapes.Filters.Create'Access);
      Holder.Register
        ("legacy-comment", Lithium.Legacy_Filters.Create'Access);
      Holder.Register
        ("pass-through", Natools.Web.Filters.Pass_Through.Create'Access);
      Holder.Register
        ("text-block", Natools.Web.Filters.Text_Blocks.Create'Access);

      Holder.Load (File_Name);

      return Result;
   end Create;

end Lithium.Dispatchers;
