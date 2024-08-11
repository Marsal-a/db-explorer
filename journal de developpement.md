---
editor_options:
  markdown:
    wrap: sentence
---

Ce document retrace les évolutions et problèmes rencontrés lors du développement de l'application shiny 'db-explorer'

## 11/08/2024

La construction du Shiny-module permettant de créer des pages de navigation est terminé, avec quelques fonctionnalités manquantes.
Rappel, nous utilisons des shiny-module afin de pouvoir générer plusieurs onglet de la page de navigation à partir du meme code.

Les fonctionnalités manquantes qui n'ont pas encore été résolus sont toutes liés à l'intéraction entre shiny et javascript.
Fonctionnalités manquantes :

-   fonction js de refocus du curseur sur l'input de filtre `navig_data_filter` lorsque l'on clique sur une celulle (et avec le "cliquer pour filtrer" activé). Le fonctionnement était l'utilisation d'un session\$sendCustomMessage :

```{r, eval=F}
session$sendCustomMessage(type="refocus",message=list("navig_data_filter"))
```

avec le code JS correspondant:

    Shiny.addCustomMessageHandler("refocus", function(e_id) {
      document.getElementById(e_id).focus();
    });

Le problème vient de l'id qui n'est pas le bon suite à l'utilisation d'un module.
Pistes : Appeler l'id du module, avec un glue::glue {NS(navig_data_filter)}...\
intégrer le code JS dans le ModuleServer

-   fonction js de refocus du curseur sur l'input de tri `navig_data_arrange` meme problème

-   fonctionalité de "remise à l'init" de l'input shiny généré par un clique sur le header du tableau.
    Un clique sur le header envoie dans une variable shiny le nom de la colonne cliqué avec la fonction callback suivante :

    ```{r}
    columnclik = NS(id,"columnClicked")
    ```

<!-- -->

    "table.on('init.dt', function() {{
      table.table().header().addEventListener('click', function(event) {{
        var target = event.target;
        console.log($(this));
        if (target.tagName === 'TH') {{
          var colIdx = $(target).index();
          var colName = table.column(colIdx).header().innerHTML;
          Shiny.setInputValue('{columnclik}', colName);
        }}
      }});
    });"
                  

Cette partie fonctionne mais nous essayons ensuite de remettre à l'init la variable, afin que l'on puisse recliquer dessus pour changer un tri ascendant en descandant et vice-versa.

On essai la fonction suivante, qui ne fonctionne pas

```{r}
session$sendCustomMessage(type="reset_colorder_navig", string_order)

```

avec le JS intégrer au moduleServer suivant:

``` 
shinyjs::runjs(sprintf("
  Shiny.addCustomMessageHandler('reset_colorder_navig', function(string_order) {
      Shiny.setInputValue('%s', 'init_reserved_string');
  });
", "columnClicked"))
```
**Piste :**  
- https://forum.posit.co/t/adding-namespace-to-javascript-in-shiny-modules/57926   
- Récupérer le bon ID en javascript https://stackoverflow.com/questions/64647732/communicate-from-js-to-shiny-in-a-module  
- retravailler  R/JS pour correctement appeler le bon inputName  
- faire un Setinput avec la valeur de la colonne (et déclancher l'observeur) puis en JS refaire un setinput sur la valeur init.  
- faire un update de l'input de tri  par JS (+ peut être une reactiveVAL ? ) et gérer le switch asc / desc en JS directement   



