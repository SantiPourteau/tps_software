package juego.carta;

import juego.Juego;
import juego.controlador.Controlador;

import java.util.Objects;

public class CartaNumero extends Carta {
    public int numero;

    public CartaNumero(String color, int numero) {
        this.color = color;
        this.numero = numero;
    }

    protected boolean esCompatibleNumero(Carta c) {
        return this.numero == ((CartaNumero) c).numero;
    }

    public boolean esCompatible(Carta c) {
        return c.esCompatibleColor(this) || c.esCompatibleNumero(this);
    }

    public Controlador actualizarControlador(Juego juego, Controlador c){
        return c.avanzar();
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof CartaNumero)) return false;
        CartaNumero that = (CartaNumero) o;
        return numero == that.numero &&
                Objects.equals(color, that.color);
    }

    public int hashCode() {
        return Objects.hash(color, numero);
    }
}
