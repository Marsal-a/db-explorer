$(document).keydown(function (event) {

  // console.log(document.activeElement)
  if ($(".btn-success:visible" || ".shiny-bound-input:visible").is(":visible") &&
    (event.metaKey || event.ctrlKey || event.shiftKey) && event.keyCode == 13) {
    $(".btn-success:visible" || ".shiny-bound-input:visible").click();
  } else if ($(".fa-edit:visible" || ".shiny-bound-input:visible").is(":visible") &&
    event.altKey && event.keyCode == 13) {
    $(".fa-edit:visible" || ".shiny-bound-input:visible").click();
  } else if ($(".fa-question:visible" || ".shiny-bound-input:visible").is(":visible") &&
    event.keyCode == 112) {
    $(".fa-question:visible" || ".shiny-bound-input:visible").click();
  } else if ($(".fa-camera:visible" || ".shiny-bound-input:visible").is(":visible") &&
    (event.metaKey || event.ctrlKey) && event.keyCode == 80) {
    $(".fa-camera:visible" || ".shiny-bound-input:visible").click();
    event.preventDefault();
  } else if ($(".fa-download:visible" || ".shiny-bound-input:visible").is(":visible") &&
    (event.metaKey || event.ctrlKey) && event.shiftKey === false && event.keyCode == 83) {
    $(".fa-download:visible" || ".shiny-bound-input:visible").click();
    event.preventDefault();
  } else if ($("#updateDescr").is(":visible") && (event.metaKey || event.ctrlKey) && event.keyCode == 13) {
    $("#updateDescr").click();
    event.preventDefault();
  } else if ($("#rmd_read_files").is(":visible") && (event.metaKey || event.ctrlKey) && event.shiftKey === false && event.keyCode == 79) {
    $("#rmd_read_files").click();
    event.preventDefault();
  } else if ($("#r_read_files").is(":visible") && (event.metaKey || event.ctrlKey) && event.shiftKey === false && event.keyCode == 79) {
    $("#r_read_files").click();
    event.preventDefault();
  } else if ($("#rmd_save").is(":visible") && (event.metaKey || event.ctrlKey) && event.shiftKey === false && event.keyCode == 83) {
    // different because rmd_save is a link see https://stackoverflow.com/a/3738603/1974918
    document.getElementById("rmd_save").click();
    event.preventDefault();
  } else if ($("#r_save").is(":visible") && (event.metaKey || event.ctrlKey) && event.shiftKey === false && event.keyCode == 83) {
    // different because r_save is a link see https://stackoverflow.com/a/3738603/1974918
    document.getElementById("r_save").click();
    event.preventDefault();
  } else if ((event.metaKey || event.ctrlKey) && event.shiftKey && event.keyCode == 83) {
    document.getElementById("state_save").click();
    event.preventDefault();
  } else if ((event.metaKey || event.ctrlKey) && event.shiftKey && event.keyCode == 79) {
    document.getElementById("state_load").click();
    event.preventDefault();
  } else if ($("#uploadfile").is(":visible") && (event.metaKey || event.ctrlKey) &&
    event.shiftKey === false && event.keyCode == 79) {
    $("#uploadfile").click();
    event.preventDefault();
  } else if ($("#man_save_data").is(":visible") && (event.metaKey || event.ctrlKey) &&
    event.shiftKey === false && event.keyCode == 83) {
    $("#man_save_data").click();
    event.preventDefault();
  }

  // focusing in text (area) inputs
  if ($("#data_rename").is(":focus") && event.keyCode == 13) {
    $("#renameButton").click();
  } else if ($("#url_csv").is(":focus") && event.keyCode == 13) {
    $("#url_csv_load").click();
  } else if ($("#url_rds").is(":focus") && event.keyCode == 13) {
    $("#url_rds_load").click();
  } else if ($("#view_name").is(":focus") && event.keyCode == 13) {
    $("#view_store").click();
  } else if ($("#pvt_name").is(":focus") && event.keyCode == 13) {
    $("#pvt_store").click();
  } else if ($("#expl_name").is(":focus") && event.keyCode == 13) {
    $("#expl_store").click();
  //} else if ($("#sql_code").is(":focus") && event.keyCode == "Enter") {
  //  $("#run_sql").click();
  // a changer en ctrl + enter
  } else if ($("#tr_name").is(":focus") && event.keyCode == 13) {
    $("#tr_store").click();
  } else if ($("#cmb_name").is(":focus") && event.keyCode == 13) {
    $("#cmb_store").click();
  } else if ($("#man_rename_data").is(":focus") &&
    document.getElementById('man_rename_data').checked === true) {
    $("#data_rename").focus();
  } else if ($("#man_add_descr").is(":focus") &&
    document.getElementById('man_add_descr').checked === true) {
    $("#man_data_descr").focus();
  } else if ($("#show_filter").is(":focus") && $("#show_filter")[0].checked) {
    $("#data_filter").focus();
  } else if ($("#tr_change_type").next(".selectize-control").find(".focus").length > 0) {
    // can set focus for selectize input
    // https://stackoverflow.com/questions/48104027/determine-if-selectize-input-has-focus
    if ($('#tr_change_type').selectize()[0].selectize.getValue() === "recode") {
      $("#tr_recode").focus();
    } else if ($('#tr_change_type').selectize()[0].selectize.getValue() === "clip") {
      $("#tr_paste").focus();
    } else if ($('#tr_change_type').selectize()[0].selectize.getValue() === "create") {
      $("#tr_create").focus();
    }
  } else if ($("#rmd_knit").is(":visible") && document.activeElement === document.body) {
    $(".ace_text-input").focus();
  } else if ($("#r_knit").is(":visible") && document.activeElement === document.body) {
    $(".ace_text-input").focus();
  }

  // needed to address https://github.com/rstudio/shiny/issues/1916
  $("input:text").attr("spellcheck", "false");
});

