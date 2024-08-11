---
title: "journal de developpement"
author: "Adam Marsal, SSER"
date: "11/08/2024"
output: html_document
---

Ce document retrace les évolutions et problèmes rencontrés lors du développement de l'application shiny 'db-explorer'


11/08/2024 
La construction du module permettant de créer des pages de navigation est terminé, avec quelques fonctionnalités manquantes. 
Rappel, nous utilisons des shiny-module afin de pouvoir générer plusieurs onglet de la page de navigation à partir du meme code. 

Les fonctionnalités manquantes qui n'ont pas encore été résolus sont toutes liés à l'intéraction entre shiny et javascript. 
Fonctionnalités manquantes : 

- fonction js de refocus du curseur sur l'input de filtre lorsque l'on clique sur une celulle (et avec le "cliquer pour filtrer" activé). 
Le fonctionnement était l'utilisation d'un session$sendCustomMessage : 

```{r, eval=F}
session$sendCustomMessage(type="refocus",message=list("navig_data_filter"))
```

