/*
 * A full-width and full-height element that signals its size to its parents.
 */
(() => {
  if (!window.customElements || !window.HTMLElement) {
    return;
  }

  class CustomClicker extends HTMLElement {
    connectedCallback() {
      this.eventName = this.getAttribute("eventname");
      this.handleClick = () => {
        this.dispatchEvent(
          new CustomEvent(this.eventName, {
            detail: {},
            bubbles: true
          })
        );
      };
      this.addEventListener("click", this.handleClick);
    }

    static get observedAttributes() {
      return ["eventname"];
    }

    disconnectedCallback() {
      this.removeEventListener("click", this.handleClick);
    }

    attributeChangedCallback() {
      this.eventName = this.getAttribute("eventname");
    }
  }

  customElements.define("custom-clicker", CustomClicker);
})();
