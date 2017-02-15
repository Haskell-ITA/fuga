# Svuluppo

## Il gioco originale

Ci basiamo sul gioco da tavolo "[fuga dagli alieni nello spazio profondo](http://www.eftaios.com/)",
con qualche adattamento per renderlo giocabile su un computer e... senza turni!

Chi ha idee o proposte modifichi pure questo file

## Idee

* il movimento va limitato
  * gli alieni si muovono più velocemente degli umani
  * una mossa ogni n secondi?
  * cooldown?
    * una barra colorata sullo schermo
    * muovendosi viene esaurita (o consumata di un po')
    * per muoversi ancora bisogna aspettare che si ricarichi
* gli umani vedono sempre tutto, alieni compresi
  * per compensare, gli alieni sono più veloci
  * ma possono vedere la distinzione tra umani e alieni?
* ogni n (5?) secondi gli umani vengono resi visibili agli alieni
  * quindi viene fatto un broadcast della loro posizione
  * il client delgi alieni mostrerà la posizione
    * facendola svanire progressivamente?
    * mantenendola fino al prossimo aggiornamento?
* bisogna evitare che un giocatore rimanga fermo troppo a lungo
  * se un giocatore rimane fermo troppo a lungo diventa visibile
    * questo però si può aggirare facendo zig-zag tra due caselle
  * alternativa: le caselle si "scaldano" se un personaggio ci sta vicino
    * in modo inversamente proporzionale alla distanza
    * c'è una dispersione che permette alle celle di raffreddarsi, ma lentamente
    * se una casella raggiunge una certa temperatura, diventa visibile agli alieni
    * gli umani sono così costretti a muoversi in continuazione

## To Do

* [x] visualizzazione
* [x] serializzazione con cereal
* [ ] gestione degli errori nella comunicazione
* [ ] comunicazione con un adt che raccoglie tutti i possibili messaggi (quando espanderemo i messaggi possibili)
* [ ] distinzione tra alieni e umani
* [ ] stati della partita (in attesa / in corso / finita)
* [ ] aggiungere elementi a questa lista

