(*
Module: Test_Awstats
Provides unit tests and examples for the <Awstats> lens.
*)

module Test_awstats =

(* Variable: default_awstats *)
let default_awstats = "#-----------------------------------------------------------------------------
# Example: \"/pathtotools/logresolvemerge.pl *.log |\"
LogFile=\"/var/log/httpd/access_log\"   # Test Comment

# Default: W
LogType=W

Include \"/etc/awstats/awstats.conf.local\"
"

(* Test: Awstats.lns *)
test Awstats.lns get default_awstats =
    { "#comment" = "-----------------------------------------------------------------------------" }
    { "#comment" = "Example: \"/pathtotools/logresolvemerge.pl *.log |\"" }
    { "LogFile" = "\"/var/log/httpd/access_log\""
      { "#comment" = "Test Comment" }
    }
    { }
    { "#comment" = "Default: W" }
    { "LogType" = "W" }
    { }
    { "Include" = "\"/etc/awstats/awstats.conf.local\"" }

(* Test: Awstats.lns *)
test Awstats.lns put default_awstats after
    set "LogFile" "\"/var/log/httpd/%YYYY-2%MM-2%DD-2\"" ;
    rm "LogType"
  = "#-----------------------------------------------------------------------------
# Example: \"/pathtotools/logresolvemerge.pl *.log |\"
LogFile=\"/var/log/httpd/%YYYY-2%MM-2%DD-2\"   # Test Comment

# Default: W

Include \"/etc/awstats/awstats.conf.local\"
"

(* Local Variables: *)
(* mode: caml *)
(* End: *)
