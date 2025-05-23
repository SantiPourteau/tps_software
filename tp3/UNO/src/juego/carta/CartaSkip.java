package juego.carta;

import juego.Juego;
import juego.controlador.Controlador;

import java.util.Objects;

public class CartaSkip extends Carta {

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

    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof CartaSkip)) return false;
        CartaSkip that = (CartaSkip) o;
        return Objects.equals(color, that.color);
    }

    public int hashCode() {
        return Objects.hash(color);
    }
}
