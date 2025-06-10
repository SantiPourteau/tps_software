package com.example.tp4.exceptions;

/**
 * Excepción para parámetros inválidos en la creación o configuración del juego.
 * Se usa para datos mal formados, incompletos o incorrectos.
 * Extiende RuntimeException porque son errores de bajo nivel.
 */
public class InvalidGameParametersException extends RuntimeException {
    
    public InvalidGameParametersException(String message) {
        super(message);
    }
    
    public InvalidGameParametersException(String message, Throwable cause) {
        super(message, cause);
    }
} 