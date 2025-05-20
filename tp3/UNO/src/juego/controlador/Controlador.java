package juego.controlador;

import java.util.List;

import juego.anillo.Ring;

/**
 * Controlador abstracto que recorre un anillo de jugadores.
 */
public abstract class Controlador {
    protected Ring anillo;

    /**
     * Constructor inicial: crea e inicializa el anillo con los jugadores.
     */
    public Controlador(List<String> jugadores) {
        anillo = new Ring();
        for (String jugador : jugadores) {
            anillo.add(jugador);
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
    public String getJugadorActual() {
        return anillo.current();
    }
}

