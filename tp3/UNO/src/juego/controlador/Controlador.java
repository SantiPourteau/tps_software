package juego.controlador;

import java.util.List;
import java.util.stream.IntStream;

import juego.Juego;
import juego.anillo.Ring;
import juego.carta.Carta;
import juego.jugador.Jugador;

/**
 * Controlador abstracto que recorre un anillo de jugadores.
 */
public abstract class Controlador {
    protected Ring anillo;

    /**
     * Constructor inicial: crea e inicializa el anillo con los jugadores.
     */
    public Controlador(List<Jugador> jugadores) {
        anillo = new Ring();
        for (Jugador persona : jugadores) {
            anillo.add(persona);
        }
    }

    /**
     * Constructor protegido para conservar el estado del anillo al invertir.
     */
    protected Controlador(Ring anillo) {
        this.anillo = anillo;
    }

    /**
     * Avanza al siguiente jugador según la dirección.
     */
    public abstract Controlador avanzar();
    /**
     * Invierte la dirección del recorrido.
     */
    public abstract Controlador invertir();
    /**
     * Saltea un turno en la dirección actual.
     */
    public abstract Controlador saltear();

    /**
     * Obtiene el jugador actual del anillo.
     */
    public Jugador getJugadorActual() {
        return anillo.current();
    }

    public Controlador penalizarJugador(Juego juego, int numCartas){
        Jugador actual = getJugadorActual();
        IntStream.range(0, numCartas)
                .mapToObj(i -> juego.mazo.removeFirst())
                .forEach(actual::recibirCarta);
        return this;
    }
}

