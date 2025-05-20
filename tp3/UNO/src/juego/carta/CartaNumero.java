package juego.carta;

import juego.controlador.Controlador;

public class CartaNumero extends Carta {
    public String color;
    public int numero;

    public CartaNumero(String color, int numero) {
        this.color = color;
        this.numero = numero;
    }

    protected boolean esCompatibleNumero(CartaNumero c) {
        return (this.numero == c.numero);
    }

    public boolean esCompatible(Carta c) {
        return c.esCompatibleColor(this) || c.esCompatibleNumero(this);
    }

    public Controlador actualizarControlador(Controlador c){
        return c.avanzar();
    }
}
