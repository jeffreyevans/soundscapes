# soundscapes
R package for acoustic soundscape analysis

soundscapes R package with utilities for formatting and analyzing acoustic soundscape data in support of biodiversity assessment.
    
    Available functions in soundscapes are:
    
         acoustic.saturation - Calculates the acoustic saturation index following Burivalova et al., (2018)
         acoustic.summary - Calculates statistical summaries of an acoustic matrix, based on defined time-periods or
                              automatic change point detection
         auc - Area Under the Curve of acoustic metrics (by row across columns)
         change.point - Change point detection called by acoustic.summary function 
         rm.timeperiods - Removes specified time-period(s), based on date-time column, from data.frame/matrix
​

          
 **Bugs**: Users are encouraged to report bugs here. Go to [issues](https://github.com/jeffreyevans/soundscapes/issues) in the menu above, and press new issue to start a new bug report, documentation correction or feature request. You can direct questions to <jeffrey_evans@tnc.org>.

**To install the development version, run the following (requires the remotes package):**
`remotes::install_github("jeffreyevans/soundscapes")`
          
