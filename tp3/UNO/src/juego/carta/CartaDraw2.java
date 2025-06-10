package juego.carta;

import juego.controlador.Controlador;
import juego.jugador.Jugador;
import juego.Juego;

import java.util.Objects;

public class CartaDraw2 extends Carta {

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

    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof CartaDraw2)) return false;
        CartaDraw2 that = (CartaDraw2) o;
        return Objects.equals(color, that.color);
    }

    public int hashCode() {
        return Objects.hash(color);
    }
}
