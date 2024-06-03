> Save, load, share, or view state

It is convenient to work with state files if you want complete your work at another time, perhaps on another computer, or to review previous work you completed using Radiant. You can save and load the state of the Radiant app just as you would a data file. The state file (extension `.rda`) will contain (1) the data loaded in Radiant, (2) settings for the analyses you were working on, (3) and any reports or code from the _Report_ menu. To save the current state of the app to your hard-disk click the <i title='Save' class='fa fa-save'></i> icon in the navbar and then click `Save radiant state file`. To load load a previous state click the <i title='Save' class='fa fa-save'></i> icon in the navbar and the click `Load radiant state file`. 

You can also share a state file with others that would like to replicate your analyses. As an example, download and then load the state file <a href="https://radiant-rstats.github.io/docs/examples/radiant-example.state.rda" target="_blank">radiant-example.state.rda</a> as described above. You will navigate automatically to the _Data > Visualize_ tab and will see a plot. See also the _Data > View_ tab for some additional settings loaded from the state file. There is also a report in _Report > Rmd_ created using the Radiant interface. The html file <a href="https://radiant-rstats.github.io/docs/examples/radiant-example.nb.html" target="_blank">radiant-example.nb.html</a> contains the output created by clicking the `Knit report` button.

Loading and saving state also works with Rstudio. If you start Radiant from Rstudio and use <i title='Power off' class='fa fa-power-off'></i> and then click `Stop`, the `r_data` environment and the `r_info` and `r_state` lists will be put into Rstudio's global workspace. If you start radiant again from the `Addins` menu it will use `r_data`, `r_info`, and `r_state` to restore state. Also, if you load a state file directly into Rstudio it will be used when you start Radiant.

Use `Refresh` in the <i title='Power off' class='fa fa-power-off'></i> menu in the navbar to return to a clean/new state.