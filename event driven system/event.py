import numpy as np
import asyncio

class SignalProcessor:
    def __init__(self, window_size):
        """
        Initialize the SignalProcessor object.

        Args:
            window_size (int): Size of the sliding window for signal processing.
        """
        self.window_size = window_size
        self.signal_buffer = np.zeros(window_size)
        self.processed_signals = []  # List to store processed signals
        
    def add_event(self, event):
        """
        Add an event to the signal buffer and process the signal.

        Args:
            event: The event to be added to the signal buffer.
        """
        self.signal_buffer = np.roll(self.signal_buffer, -1)
        self.signal_buffer[-1] = event
        self.process_signal()
    
    def process_signal(self):
        """
        Process the signal using a moving average filter.

        This method can be extended to include more sophisticated signal processing algorithms.
        """
        signal_mean = np.mean(self.signal_buffer)
        self.processed_signals.append(signal_mean)  # Store processed signal
        
        signal_fft = np.fft.fft(self.signal_buffer)
        self.processed_signals.append(signal_fft)  # Store processed signal
        
        
        signal_variance = np.var(self.signal_buffer)
        self.processed_signals.append(signal_variance)  
        
        #   Calculate signal maximum
        signal_max = np.max(self.signal_buffer)
        self.processed_signals.append(signal_max)  
        
        #   Calculate signal minimum
        signal_min = np.min(self.signal_buffer)
        self.processed_signals.append(signal_min) 
        
        print("Processed Signal (Moving Average):", signal_mean)
        print("Processed Signal (FFT):", signal_fft)
        print("Processed Signal (Variance):", signal_variance)
        print("Processed Signal (Maximum):", signal_max)
        print("Processed Signal (Minimum):", signal_min)
        
    def get_processed_signals(self):
        """
        Return the list of processed signals.

        Returns:
            List: List of processed signals.
        """
        return self.processed_signals

async def main():
    processor = SignalProcessor(window_size=5)  

    while True:
        event = np.random.randint(0, 100)  
        processor.add_event(event)
        await asyncio.sleep(1)  

asyncio.run(main())