package juego.carta;

import juego.Juego;
import juego.controlador.Controlador;
import java.util.Objects;

public class CartaReverse extends Carta {

    public CartaReverse(String color) {
        this.color = color;
    }

    protected boolean esCompatibleReverse(Carta c){
        return true;
    }

    public boolean esCompatible(Carta c) {
        return this.color.equals(c.color) || c.esCompatibleReverse(this);
    }

    public Controlador actualizarControlador(Juego juego, Controlador c){
        return c.invertir().avanzar();
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof CartaReverse)) return false;
        CartaReverse that = (CartaReverse) o;
        return Objects.equals(color, that.color);
    }

    public int hashCode() {
        return Objects.hash(color);
    }
}