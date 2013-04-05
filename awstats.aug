(*
Module: Awstats
Parses /etc/awstats.conf and /etc/awstats.d/*

Author: David Lutterkort <lutter@redhat.com>

About: Reference

About: License
This file is licenced under the LGPL v2+, like the rest of Augeas.

About: Lens Usage
To be documented

About: Configuration files
This lens applies to /etc/awstats.conf and /etc/awstats.d/*. See <filter>.

About: Examples
The <Test_Awstats> file contains various examples and tests.
*)

module Awstats =
autoload xfm

let comment  = Util.comment_generic /[ \t]*[#][ \t]*/ "# "
let empty    = Util.empty
let eol      = Util.eol
let equal    = Sep.equal
let kvl      = Build.key_value_line
let space    = Sep.space
let word     = Rx.word

(* Space separated with quotes
  KEY "VALUE" *)
let sp_sep_quot (kw:regexp) = 
  let value = store /"[^ \t\n]+"/ in 
    [ key kw . space . value . eol ]

(* Equal separated with quotes
  KEY="VALUE" *)
let eq_sep_quot (kw:regexp) = 
  let value = store /"[^ \t\n]+"/ in 
    [ key kw . equal . value . eol ]

(* Equal separated without quotes
  KEY=VALUE *)
let eq_sep (kw:regexp) = 
  let value = store /[^ \t\n]+/ in 
    [ key kw . equal . value . eol ]

(* List of space separated with quotes keys *)
let sp_sep_quot_list = "Include"

(* List of equal separated with quotes keys *)
let eq_sep_quot_list = "LogFile"
| "LogSeparator"
| "SiteDomain"
| "HostAliases"
| "DirData"
| "DirCgi"
| "DirIcons"
| "DNSStaticCacheFile"
| "DNSLastUpdateCacheFile"
| "SkipDnsLookupsFor"
| "AllowAccessFromWebToFollowingAuthenticatedUsers"
| "AllowAccessFromWebToFollowingIPAddresses"
| "DefaultFile"
| "SkipHosts"
| "SkipUserAgents"
| "SkipFiles"
| "SkipReferrersBlackList"
| "OnlyHosts"
| "OnlyUserAgents"
| "OnlyUsers"
| "OnlyFiles"
| "NotPageList"
| "ValidHTTPCodes"
| "ValidSMTPCodes"
| "URLQuerySeparators"
| "URLWithQueryWithOnlyFollowingParameters"
| "URLWithQueryWithoutFollowingParameters"
| "ErrorMessages"
| "WrapperScript"
| "MiscTrackerUrl"
| "Lang"
| "DirLang"
| "ShowFlagLinks"
| "UseHTTPSLinkForUrl"
| "HTMLHeadSection"
| "HTMLEndSection"
| "Logo"
| "LogoLink"
| "StyleSheet"
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

(* List of equal separated with quotes keys *)
let eq_sep_list = "AddDataArrayMonthStats"
| "AddDataArrayShowDaysOfMonthStats"
| "AddDataArrayShowDaysOfWeekStats"
| "AddDataArrayShowHoursStats"
| "AllowAccessFromWebToAuthenticatedUsersOnly"
| "AllowFullYearView"
| "AllowToUpdateStatsFromBrowser"
| "ArchiveLogRecords"
| "AuthenticatedUsersNotCaseSensitive"
| "BarHeight  "
| "BarWidth   "
| "BuildHistoryFormat"
| "BuildReportFormat"
| "CreateDirDataIfNotExists"
| "DebugMessages"
| "DecodeUA"
| "DetailedReportsOnNewWindows"
| "DNSLookup"
| "EnableLockForUpdate"
| "Expires"
| "ExtraTrackedRowsLimit"
| "FirstDayOfWeek"
| "IncludeInternalLinksInOriginSection"
| "KeepBackupOfHistoricFiles"
| "LevelForBrowsersDetection"
| "LevelForFileTypesDetection"
| "LevelForKeywordsDetection"
| "LevelForOSDetection"
| "LevelForRefererAnalyze"
| "LevelForRobotsDetection"
| "LevelForSearchEnginesDetection"
| "LevelForWormsDetection"
| "LogFormat"
| "LogType"
| "MaxLengthOfShownURL"
| "MaxNbOfBrowsersShown "
| "MaxNbOfDomain "
| "MaxNbOfDownloadsShown "
| "MaxNbOfEMailsShown "
| "MaxNbOfHostsShown "
| "MaxNbOfKeyphrasesShown "
| "MaxNbOfKeywordsShown "
| "MaxNbOfLoginShown "
| "MaxNbOfOsShown "
| "MaxNbOfPageShown "
| "MaxNbOfRefererShown "
| "MaxNbOfRobotShown "
| "MaxNbOfScreenSizesShown "
| "MaxNbOfWindowSizesShown "
| "MaxRowsInHTMLOutput"
| "MetaRobot"
| "MinHitBrowser "
| "MinHitDomain  "
| "MinHitDownloads "
| "MinHitEMail   "
| "MinHitFile    "
| "MinHitHost    "
| "MinHitKeyphrase "
| "MinHitKeyword "
| "MinHitLogin   "
| "MinHitOs      "
| "MinHitRefer   "
| "MinHitRobot   "
| "MinHitScreenSize "
| "MinHitWindowSize "
| "NbOfLinesForCorruptedLog"
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
| "URLNotCaseSensitive"
| "URLReferrerWithQuery"
| "URLWithAnchor"
| "URLWithQuery"
| "UseFramesWhenCGI"
| "WarningMessages"

(* Variable: filter *)
let filter = incl "/etc/awstats/awstats.*"

(* View: lns
The awstats lens *)
let lns = ( empty
| comment
| sp_sep_quot (sp_sep_quot_list)
| eq_sep_quot (eq_sep_quot_list)
| eq_sep (eq_sep_list))*

let xfm = transform lns filter
