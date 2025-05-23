package juego.carta;

import juego.Juego;
import juego.controlador.Controlador;

public class CartaSkip extends Carta {
    public String color;

    public CartaSkip(String color) {
        this.color = color;
    }

    public boolean esCompatibleSkip(Carta c){
        return true;
    }

    public boolean esCompatible(Carta c) {
        return (this.color.equals(c.color) || c.esCompatibleSkip(this));
    }

    public Controlador actualizarControlador(Juego juego, Controlador c){
        return c.saltear();
    }
}
