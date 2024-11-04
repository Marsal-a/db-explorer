/* Script permettrant de copier du texte dans le presse-papier  */
/* Inspired from https://stackoverflow.com/questions/78448126/copying-output-to-clipboard-in-r-shiny */
Shiny.addCustomMessageHandler('copyToClipboard', function (txt) {
                navigator.clipboard.writeText(txt);
});