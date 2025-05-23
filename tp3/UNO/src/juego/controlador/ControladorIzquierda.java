package juego.controlador;

import java.util.List;
import juego.anillo.Ring;
import juego.carta.Carta;
import juego.jugador.Jugador;
import juego.Juego;

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

    public Controlador avanzar() {
        anillo.previous();
        return this;
    }

    public Controlador invertir() {
        return new ControladorDerecha(anillo);
    }

    public Controlador saltear() {
        anillo.previous();
        anillo.previous();
        return this;
    }

    public Controlador penalizarJugador(Juego juego, int numCartas) {
        Jugador actual = anillo.current();
        for (int i = 0; i < numCartas; i++) {
            Carta carta = juego.mazo.removeFirst();
            actual.recibirCarta(carta);
        }
        return this;
    }
}