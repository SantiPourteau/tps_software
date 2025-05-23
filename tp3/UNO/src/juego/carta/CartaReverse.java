package juego.carta;

import juego.Juego;
import juego.controlador.Controlador;

public class CartaReverse extends Carta {
    public String color;

    public CartaReverse(String color) {
        this.color = color;
    }

    protected boolean esCompatibleReverse(CartaReverse c){
        return true;
    }

    public boolean esCompatible(Carta c) {
        return this.color.equals(c.color) || c.esCompatibleReverse(this);
    }

    public Controlador actualizarControlador(Juego juego, Controlador c){
        return c.invertir().avanzar();
    }
}