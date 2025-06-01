document.addEventListener('DOMContentLoaded', function() {
  const slides = document.querySelectorAll('.carousel-content p');
  const prevButton = document.querySelector('.carousel .prev');
  const nextButton = document.querySelector('.carousel .next');
  let currentIndex = 0;

  // Mostrar el primer slide al inicio
  slides[currentIndex].classList.add('active');

  // Función para mostrar slide según el índice
  function showSlide(index) {
    slides.forEach((slide, i) => {
      slide.classList.toggle('active', i === index);
    });
  }

  // Botón anterior
  prevButton.addEventListener('click', () => {
    currentIndex = (currentIndex - 1 + slides.length) % slides.length;
    showSlide(currentIndex);
  });

  // Botón siguiente
  nextButton.addEventListener('click', () => {
    currentIndex = (currentIndex + 1) % slides.length;
    showSlide(currentIndex);
  });
});
