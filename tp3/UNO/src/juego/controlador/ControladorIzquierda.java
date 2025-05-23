package juego.controlador;

import java.util.List;
import juego.anillo.Ring;
import juego.jugador.Jugador;

/**
 * Controlador que avanza en sentido antihorario (izquierda).
 */
public class ControladorIzquierda extends Controlador {
    public ControladorIzquierda(List<Jugador> jugadores) {
        super(jugadores);
    }
    protected ControladorIzquierda(Ring anillo) {
        super(anillo);
    }

    @Override
    public Controlador avanzar() {
        anillo.previous();
        return this;
    }

    @Override
    public Controlador invertir() {
        return new ControladorDerecha(anillo);
    }

    @Override
    public Controlador saltear() {
        anillo.previous();
        anillo.previous();
        return this;
    }
}