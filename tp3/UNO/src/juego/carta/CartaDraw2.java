package juego.carta;

import juego.controlador.Controlador;
import juego.jugador.Jugador;
import juego.Juego;

public class CartaDraw2 extends Carta {
    public String color;

    public CartaDraw2(String color) {
        this.color = color;
    }

    public boolean esCompatibleDraw2(Carta c){
        return true;
    }

    public boolean esCompatible(Carta c) {
        return (this.color.equals(c.color) || c.esCompatibleDraw2(this));
    }

    public Controlador actualizarControlador(Juego juego, Controlador c){
        return c.avanzar().penalizarJugador(juego, 2).avanzar();
    }

    public int penalizar() {
        return 2;
    }
}
