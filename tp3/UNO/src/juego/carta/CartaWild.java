package juego.carta;

import juego.Juego;
import juego.controlador.Controlador;

public class CartaWild extends Carta {
    public String color;

    public CartaWild() {
        this.color = "wild";
    }
    public CartaWild(String color) {
        this.color = color;
    }

    public boolean esCompatible(Carta c) {
        return true;
    }

    public Controlador actualizarControlador(Juego juego, Controlador c){
        return c.avanzar();
    }
}
