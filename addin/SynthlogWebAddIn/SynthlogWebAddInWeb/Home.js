(function () {
    "use strict";

    var cellToHighlight;
    var messageBanner;

    Office.onReady(function () {
        $(document).ready(function () {
        });
    });
    // La fonction d'initialisation doit être exécutée chaque fois qu'une nouvelle page est chargée.
    /*
     * Office.initialize = function (reason) {
        $(document).ready(function () {
            // Initialiser le mécanisme de notification de FabricUI, et le masquer
            // var element = document.querySelector('.ms-MessageBanner');
            // messageBanner = new fabric.MessageBanner(element);
            // messageBanner.hideBanner();
            
            // Si Excel 2016 n'est pas utilisé, employez la logique de secours.
            /*
             * if (!Office.context.requirements.isSetSupported('ExcelApi', '1.1')) {
                $("#template-description").text("Cet exemple affiche la valeur des cellules que vous avez sélectionnées dans la feuille de calcul.");
                $('#button-text').text("Afficher !");
                $('#button-desc').text("Afficher la sélection");

                $('#highlight-button').click(displaySelectedCells);
                return;
            }
            *

            $("#template-description").text("Cet exemple met en surbrillance la valeur la plus élevée des cellules que vous avez sélectionnées dans la feuille de calcul.");
            $('#button-text').text("Mettre en surbrillance !");
            $('#button-desc').text("Met en surbrillance le nombre le plus long.");
                
            // loadSampleData();

            // Ajoutez un gestionnaire d'événements Click pour le bouton de mise en surbrillance.
            $('#highlight-button').click(hightlightHighestValue);
        });
    };
    */

    function loadSampleData() {
        var values = [
            [Math.floor(Math.random() * 1000), Math.floor(Math.random() * 1000), Math.floor(Math.random() * 1000)],
            [Math.floor(Math.random() * 1000), Math.floor(Math.random() * 1000), Math.floor(Math.random() * 1000)],
            [Math.floor(Math.random() * 1000), Math.floor(Math.random() * 1000), Math.floor(Math.random() * 1000)]
        ];

        // Exécutez une opération de traitement par lots sur le modèle objet Excel
        Excel.run(function (ctx) {
            // Créez un objet proxy pour la feuille active
            var sheet = ctx.workbook.worksheets.getActiveWorksheet();
            // Mettre en file d'attente une commande pour écrire les exemples de données dans la feuille de calcul
            sheet.getRange("B3:D5").values = values;

            // Exécuter les commandes mises en file d'attente, puis retourner une promesse pour indiquer l'achèvement de la tâche
            return ctx.sync();
        })
        .catch(errorHandler);
    }

    function hightlightHighestValue() {
        // Exécutez une opération de traitement par lots sur le modèle objet Excel
        Excel.run(function (ctx) {
            // Créer un objet proxy pour la plage sélectionnée, puis charger ses propriétés
            var sourceRange = ctx.workbook.getSelectedRange().load("values, rowCount, columnCount");

            // Exécuter la commande mise en file d'attente, puis retourner une promesse pour indiquer l'achèvement de la tâche
            return ctx.sync()
                .then(function () {
                    var highestRow = 0;
                    var highestCol = 0;
                    var highestValue = sourceRange.values[0][0];

                    // Rechercher la cellule à mettre en surbrillance
                    for (var i = 0; i < sourceRange.rowCount; i++) {
                        for (var j = 0; j < sourceRange.columnCount; j++) {
                            if (!isNaN(sourceRange.values[i][j]) && sourceRange.values[i][j] > highestValue) {
                                highestRow = i;
                                highestCol = j;
                                highestValue = sourceRange.values[i][j];
                            }
                        }
                    }

                    cellToHighlight = sourceRange.getCell(highestRow, highestCol);
                    sourceRange.worksheet.getUsedRange().format.fill.clear();
                    sourceRange.worksheet.getUsedRange().format.font.bold = false;

                    // Mettre la cellule en surbrillance
                    cellToHighlight.format.fill.color = "orange";
                    cellToHighlight.format.font.bold = true;
                })
                .then(ctx.sync);
        })
        .catch(errorHandler);
    }

    function displaySelectedCells() {
        Office.context.document.getSelectedDataAsync(Office.CoercionType.Text,
            function (result) {
                if (result.status === Office.AsyncResultStatus.Succeeded) {
                    showNotification('Le texte sélectionné est :', '"' + result.value + '"');
                } else {
                    showNotification('Erreur', result.error.message);
                }
            });
    }

    // Fonction d'assistance pour le traitement des erreurs
    function errorHandler(error) {
        // Veillez toujours à intercepter les erreurs accumulées qui apparaissent après l'exécution d'Excel.run
        showNotification("Erreur", error);
        console.log("Error: " + error);
        if (error instanceof OfficeExtension.Error) {
            console.log("Debug info: " + JSON.stringify(error.debugInfo));
        }
    }

    // Fonction d'assistance pour afficher les notifications
    function showNotification(header, content) {
        $("#notification-header").text(header);
        $("#notification-body").text(content);
        messageBanner.showBanner();
        messageBanner.toggleExpansion();
    }
})();
