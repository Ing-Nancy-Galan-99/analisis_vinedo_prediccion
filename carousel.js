const slides = document.querySelectorAll('.carousel-content p');
const prev = document.getElementById('prev');
const next = document.getElementById('next');
let index = 0;

function showSlide(i) {
  slides.forEach((slide, idx) => {
    slide.classList.toggle('active', idx === i);
  });
}

showSlide(index);

prev.addEventListener('click', () => {
  index = (index - 1 + slides.length) % slides.length;
  showSlide(index);
});

next.addEventListener('click', () => {
  index = (index + 1) % slides.length;
  showSlide(index);
});

// Rotación automática cada 5 segundos
setInterval(() => {
  index = (index + 1) % slides.length;
  showSlide(index);
}, 5000);
