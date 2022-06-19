// See the Tailwind configuration guide for advanced usage
// https://tailwindcss.com/docs/configuration
module.exports = {
  content: ["./js/**/*.js", "../lib/*_web.ex", "../lib/*_web/**/*.*ex"],
  theme: {
    extend: {},
    fontFamily: {
      sans: ["Poppins", "sans-serif"],
    },
  },
  plugins: [require("@tailwindcss/forms", "daisyui")],
};
