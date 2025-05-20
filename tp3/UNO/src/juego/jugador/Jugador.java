package juego.jugador;

import juego.carta.Carta;

import java.util.List;

public class Jugador {

    private List<Carta> mano;

    public Jugador(List<Carta> cartas) {
        this.mano = cartas;
    }

    public Jugador jugarCarta(Carta carta){
        // saca la carta de la lista si está, si no devuelve error (SIN USAR IFS)
    }

}
