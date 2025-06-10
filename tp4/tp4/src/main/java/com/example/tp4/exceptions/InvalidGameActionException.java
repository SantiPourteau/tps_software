package com.example.tp4.exceptions;

/**
 * Excepción para jugadas inválidas en el juego UNO.
 * Se usa para situaciones como jugador inexistente, carta incorrecta, turno incorrecto, etc.
 * Extiende IllegalArgumentException porque son problemas a nivel de negocio.
 */
public class InvalidGameActionException extends IllegalArgumentException {
    
    public InvalidGameActionException(String message) {
        super(message);
    }
    
    public InvalidGameActionException(String message, Throwable cause) {
        super(message, cause);
    }
} 