package juego.controlador;

import java.util.List;

import juego.Juego;
import juego.anillo.Ring;
import juego.carta.Carta;
import juego.jugador.Jugador;
/**
 * Controlador que avanza en sentido horario (derecha).
 */
public class ControladorDerecha extends Controlador {
    public ControladorDerecha(List<Jugador> jugadores) {
        super(jugadores);
    }
    protected ControladorDerecha(Ring anillo) {super(anillo);}

    public Controlador avanzar() {
        anillo.previous();
        return this;
    }

    public Controlador invertir() {
        return new ControladorIzquierda(anillo);
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
