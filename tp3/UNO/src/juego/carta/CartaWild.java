package juego.carta;

import juego.Juego;
import juego.controlador.Controlador;

import java.util.Objects;

public class CartaWild extends Carta {

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

    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof CartaWild)) return false;
        if (((CartaWild) o).color.equals("wild")) return true;
        else{
            CartaWild that = (CartaWild) o;
            return Objects.equals(color, that.color);
        }
    }

    public int hashCode() {
        return Objects.hash(color);
    }
}
