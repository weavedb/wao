// Smart Language Switcher for Vocs
(function() {
  if (typeof window === 'undefined') return;

  console.log('Language switcher script loaded');

  // Use global click handler with capture to intercept before Vocs routing
  document.addEventListener('click', function(e) {
    const target = e.target.closest('a[href]');
    if (!target) return;

    const href = target.getAttribute('href');
    console.log('Link clicked:', href);

    // Only handle language switcher links
    if (href === '/getting-started' ||
        href === '/zh/getting-started' ||
        href === '/machine/getting-started') {

      console.log('Language switcher link detected!');
      e.preventDefault();
      e.stopPropagation();
      e.stopImmediatePropagation();

      const currentPath = window.location.pathname;
      let targetLang = '';
      let basePath = currentPath;

      // Determine target language from clicked link
      if (href.startsWith('/zh/')) {
        targetLang = '/zh';
      } else if (href.startsWith('/machine/')) {
        targetLang = '/machine';
      } else {
        targetLang = '';
      }

      // Remove current language prefix from path
      if (currentPath.startsWith('/zh/')) {
        basePath = currentPath.replace(/^\/zh/, '');
      } else if (currentPath.startsWith('/machine/')) {
        basePath = currentPath.replace(/^\/machine/, '');
      }

      // Ensure basePath starts with /
      if (!basePath.startsWith('/')) {
        basePath = '/' + basePath;
      }

      // If basePath is just '/', use '/getting-started' as fallback
      if (basePath === '/') {
        basePath = '/getting-started';
      }

      // Construct new path
      const newPath = targetLang + basePath;

      console.log('Language switch:', {
        currentPath,
        targetLang,
        basePath,
        newPath
      });

      // Navigate to new path
      window.location.href = newPath;
    }
  }, true); // Use capture phase to intercept before other handlers

  console.log('Language switcher initialized');
})();
