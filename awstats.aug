(*
Module: Awstats
Parses /etc/awstats.*

Author: Damon Conway <damon.conway@alchemysystems.com>

About: Reference

About: License
This file is licenced under the LGPL v2+, like the rest of Augeas.

About: Lens Usage
To be documented

About: Configuration files
This lens applies to /etc/awstats.*.  See <filter>.

About: Examples
The <Test_Awstats> file contains various examples and tests.
*)
module Awstats =
autoload xfm

let comment        = Util.comment_generic /^[ \t]*#[ \t]*/ "# "
let comment_or_eol = Util.comment_or_eol
let op_delim (op:string) = del (/[ \t]*/ . op . /[ \t]*/) (" " . op . " ")
let empty          = Util.empty
let eol            = Util.eol
(* let equal          = Sep.equal *)
let equal          = op_delim "="
let indent         = Util.indent
let space          = Sep.space
let word           = /[^ \t\n]+/
let qword          = /"[^ \t\n]+"/
(* Shamelessly stolen from raphink's Modprobe *)
let sto_to_eol     = /(([^# \t\n\\\\][^#\n\\\\]*[ \t]*\\\\[ \t]*\n[ \t]*)*([^# \t\n\\\\][^#\n\\\\]*[^# \t\n\\\\]|[^# \t\n\\\\])|[^# \t\n\\\\])/

(* Space separated
  KEY "VALUE" *)
let sp_sep (kw:regexp) = 
  let value = store sto_to_eol in 
    [ key kw . space . value . comment_or_eol ]

(* Equal separated
  KEY="VALUE"
  KEY=VALUE
*)
let simple_entry (kw:regexp) = 
  let value = store sto_to_eol in 
    [ key kw . equal . value . comment_or_eol ]

(* List of equal separated with quotes keys *)
let simple_list = "AddDataArrayMonthStats"
| "AddDataArrayShowDaysOfMonthStats"
| "AddDataArrayShowDaysOfWeekStats"
| "AddDataArrayShowHoursStats"
| "AllowAccessFromWebToAuthenticatedUsersOnly"
| "AllowAccessFromWebToFollowingAuthenticatedUsers"
| "AllowAccessFromWebToFollowingIPAddresses"
| "AllowFullYearView"
| "AllowToUpdateStatsFromBrowser"
| "ArchiveLogRecords"
| "AuthenticatedUsersNotCaseSensitive"
| "BarHeight"
| "BarWidth"
| "BuildHistoryFormat"
| "BuildReportFormat"
| "color_Background"
| "color_TableBGTitle"
| "color_TableTitle"
| "color_TableBG"
| "color_TableRowTitle"
| "color_TableBGRowTitle"
| "color_TableBorder"
| "color_text"
| "color_textpercent"
| "color_titletext"
| "color_weekend"
| "color_link"
| "color_hover"
| "color_u"
| "color_v"
| "color_p"
| "color_h"
| "color_k"
| "color_s"
| "color_e"
| "color_x"
| "CreateDirDataIfNotExists"
| "DebugMessages"
| "DecodeUA"
| "DefaultFile"
| "DetailedReportsOnNewWindows"
| "DirCgi"
| "DirData"
| "DirIcons"
| "DirLang"
| "DNSLastUpdateCacheFile"
| "DNSLookup"
| "DNSStaticCacheFile"
| "EnableLockForUpdate"
| "ErrorMessages"
| "Expires"
| "ExtraTrackedRowsLimit"
| "FirstDayOfWeek"
| "HostAliases"
| "HTMLHeadSection"
| "HTMLEndSection"
| "IncludeInternalLinksInOriginSection"
| "KeepBackupOfHistoricFiles"
| "Lang"
| "LevelForBrowsersDetection"
| "LevelForFileTypesDetection"
| "LevelForKeywordsDetection"
| "LevelForOSDetection"
| "LevelForRefererAnalyze"
| "LevelForRobotsDetection"
| "LevelForSearchEnginesDetection"
| "LevelForWormsDetection"
| "LogFormat"
| "LogFile"
| "LogSeparator"
| "LogType"
| "Logo"
| "LogoLink"
| "MaxLengthOfShownURL"
| "MaxNbOfBrowsersShown"
| "MaxNbOfDomain"
| "MaxNbOfDownloadsShown"
| "MaxNbOfEMailsShown"
| "MaxNbOfHostsShown"
| "MaxNbOfKeyphrasesShown"
| "MaxNbOfKeywordsShown"
| "MaxNbOfLoginShown"
| "MaxNbOfOsShown"
| "MaxNbOfPageShown"
| "MaxNbOfRefererShown"
| "MaxNbOfRobotShown"
| "MaxNbOfScreenSizesShown"
| "MaxNbOfWindowSizesShown"
| "MaxRowsInHTMLOutput"
| "MetaRobot"
| "MinHitBrowser"
| "MinHitDomain"
| "MinHitDownloads"
| "MinHitEMail"
| "MinHitFile"
| "MinHitHost"
| "MinHitKeyphrase"
| "MinHitKeyword"
| "MinHitLogin"
| "MinHitOs"
| "MinHitRefer"
| "MinHitRobot"
| "MinHitScreenSize"
| "MinHitWindowSize"
| "MiscTrackerUrl"
| "NbOfLinesForCorruptedLog"
| "NotPageList"
| "OnlyHosts"
| "OnlyFiles"
| "OnlyUsers"
| "OnlyUserAgents"
| "PurgeLogFile"
| "SaveDatabaseFilesWithPermissionsForEveryone"
| "ShowAuthenticatedUsers"
| "ShowBrowsersStats"
| "ShowClusterStats"
| "ShowDaysOfMonthStats"
| "ShowDaysOfWeekStats"
| "ShowDomainsStats"
| "ShowDownloadsStats"
| "ShowEMailReceivers"
| "ShowEMailSenders"
| "ShowFileSizesStats"
| "ShowFileTypesStats"
| "ShowFlagLinks"
| "ShowHostsStats"
| "ShowHoursStats"
| "ShowHTTPErrorsStats"
| "ShowKeyphrasesStats"
| "ShowKeywordsStats"
| "ShowLinksOnUrl"
| "ShowMenu"
| "ShowMiscStats"
| "ShowMonthStats"
| "ShowOriginStats"
| "ShowOSStats"
| "ShowPagesStats"
| "ShowRobotsStats"
| "ShowScreenSizeStats"
| "ShowSessionsStats"
| "ShowSMTPErrorsStats"
| "ShowSummary"
| "ShowWormsStats"
| "SiteDomain"
| "SkipDNSLookupFor"
| "SkipFiles"
| "SkipHosts"
| "SkipReferrersBlackList"
| "SkipUserAgents"
| "StyleSheet"
| "URLNotCaseSensitive"
| "URLReferrerWithQuery"
| "URLQuerySeparators"
| "URLWithAnchor"
| "URLWithQuery"
| "URLWithQueryWithOnlyFollowingParameters"
| "URLWithQueryWithoutFollowingParameters"
| "UseFramesWhenCGI"
| "UseHTTPSLinkForUrl"
| "ValidHTTPCodes"
| "ValidSMTPCodes"
| "WarningMessages"
| "WrapperScript"

(* View: entry *)
let entry = ( sp_sep /Include/ | simple_entry (simple_list) )

(* Variable: filter *)
let filter = incl "/etc/awstats/*"
           . Util.stdexcl

(* View: lns
The awstats lens *)
let lns = (empty | comment | entry)*

let xfm = transform lns filter
