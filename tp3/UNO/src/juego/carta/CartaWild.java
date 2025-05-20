package juego.carta;

import juego.controlador.Controlador;

public class CartaWild extends Carta {
    public String color;

    public CartaWild(String color) {
        this.color = color;
    }

    public boolean esCompatible(Carta c) {
        return true;
    }

    public Controlador actualizarControlador(Controlador c){
        return c.avanzar();
    }
}
