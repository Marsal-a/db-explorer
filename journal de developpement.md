
Ce document retrace les évolutions et problèmes rencontrés lors du développement de l'application shiny 'db-explorer'


### 11/08/2024 
La construction du module permettant de créer des pages de navigation est terminé, avec quelques fonctionnalités manquantes. 
Rappel, nous utilisons des shiny-module afin de pouvoir générer plusieurs onglet de la page de navigation à partir du meme code. 

Les fonctionnalités manquantes qui n'ont pas encore été résolus sont toutes liés à l'intéraction entre shiny et javascript. 
Fonctionnalités manquantes : 

- fonction js de refocus du curseur sur l'input de filtre `navig_data_filter` lorsque l'on clique sur une celulle (et avec le "cliquer pour filtrer" activé). 
Le fonctionnement était l'utilisation d'un session$sendCustomMessage : 
```{r, eval=F}

session$sendCustomMessage(type="refocus",message=list("navig_data_filter"))
```
avec le code JS correspondant. 
```
Shiny.addCustomMessageHandler("refocus", function(e_id) {
  document.getElementById(e_id).focus();
});

```
Le problème vient de l'id qui n'est pas le bon suite à l'utilisation d'un module. 
Pistes : 
Appeler l'id du module, avec un glue::glue {NS(navig_data_filter)}...   
intégrer le code JS dans le ModuleServer


- fonction js de refocus du curseur sur l'input de tri `navig_data_arrange`



