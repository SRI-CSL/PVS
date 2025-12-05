#!/usr/bin/env python3

import argparse
import sys
from abc import ABC, abstractmethod
from pathlib import Path
from typing import Optional

from prompt_toolkit import PromptSession
from prompt_toolkit.history import InMemoryHistory
from rich.console import Console
from rich.panel import Panel
from rich.syntax import Syntax

import warnings
warnings.filterwarnings("ignore", category=UserWarning)

class Backend(ABC):
    """Abstract base class for backend processors."""

    @abstractmethod
    def process(self, llm_output: str) -> str:
        """Process LLM output and return a result."""
        pass

    @property
    @abstractmethod
    def output_language(self) -> str:
        """Return the syntax highlighting language for the output."""
        pass


class JSONBackend(Backend):
    """Example backend that processes JSON output."""

    def process(self, llm_output: str) -> str:
        """Echo the LLM output as JSON (stub implementation)."""
        return f'{{"status": "processed", "length": {len(llm_output)}, "preview": "{llm_output[:50]}..."}}'

    @property
    def output_language(self) -> str:
        return "json"


# Backend registry
BACKENDS = {
    "json": JSONBackend,
}


class LLMInterface:
    """Main interface for interacting with HuggingFace models."""

    def __init__(
        self,
        model_name: str,
        system_prompt: str,
        backend: Backend,
        console: Console,
        use_gguf: bool = False
    ):
        self.model_name = model_name
        self.system_prompt = system_prompt
        self.backend = backend
        self.console = console
        self.use_gguf = use_gguf
        self.model = None
        self.tokenizer = None
        self.generator = None
        self.llama_model = None

        self.console.print(f"[bold blue]Loading model:[/bold blue] {model_name}")

        if use_gguf:
            self._load_gguf_model()
        else:
            self._load_transformers_model()

    def _load_gguf_model(self):
        """Load a GGUF model using llama-cpp-python."""
        try:
            from huggingface_hub import hf_hub_download

            # Download GGUF file from HuggingFace
            self.console.print("[yellow]Downloading GGUF model file...[/yellow]")

            # For GGUF models, we need to download the actual .gguf file
            # The model_name should be in format: "repo/model-name"
            repo_id = self.model_name

            # Try to find .gguf file in the repo
            from huggingface_hub import list_repo_files
            files = list_repo_files(repo_id)
            gguf_files = [f for f in files if f.endswith('.gguf')]

            if not gguf_files:
                self.console.print(f"[bold red]Error:[/bold red] No .gguf files found in {repo_id}")
                sys.exit(1)

            # Use the first .gguf file found
            gguf_file = gguf_files[0]
            self.console.print(f"[yellow]Using file: {gguf_file}[/yellow]")

            model_path = hf_hub_download(
                repo_id=repo_id,
                filename=gguf_file
            )

            self.console.print("[yellow]Loading model into memory...[/yellow]")
            from llama_cpp import Llama
            self.llama_model = Llama(
                model_path=model_path,
                n_ctx=4096,  # Context window
                n_threads=8,  # CPU threads
                n_gpu_layers=0,  # 0 for CPU-only
                verbose=False
            )

            self.console.print("[bold green]✓[/bold green] Model loaded successfully\n")
        except Exception as e:
            self.console.print(f"[bold red]Error loading model:[/bold red] {e}")
            import traceback
            traceback.print_exc()
            sys.exit(1)

    def _load_transformers_model(self):
        """Load a standard transformers model."""
        try:
            from transformers import AutoModelForCausalLM, AutoTokenizer, pipeline
            self.tokenizer = AutoTokenizer.from_pretrained(self.model_name)
            self.model = AutoModelForCausalLM.from_pretrained(self.model_name)

            # Create pipeline for text generation
            self.generator = pipeline(
                "text-generation",
                model=self.model,
                tokenizer=self.tokenizer,
                max_new_tokens=256,
                do_sample=True,
                temperature=0.7,
            )

            self.console.print("[bold green]✓[/bold green] Model loaded successfully\n")
        except Exception as e:
            self.console.print(f"[bold red]Error loading model:[/bold red] {e}")
            sys.exit(1)

    def generate_response(self, user_prompt: str) -> str:
        """Generate a response from the LLM."""
        if self.use_gguf:
            return self._generate_gguf(user_prompt)
        else:
            return self._generate_transformers(user_prompt)

    def _generate_gguf(self, user_prompt: str) -> str:
        """Generate response using llama.cpp."""
        # Combine system prompt with user prompt
        full_prompt = f"{self.system_prompt}\n\nUser: {user_prompt}\n\nAssistant:"

        try:
            response = self.llama_model(
                full_prompt,
                max_tokens=30000,
                temperature=0.7,
                top_p=0.9,
                echo=False,
                stop=["User:", "\n\n"]
            )
            return response["choices"][0]["text"].strip()
        except Exception as e:
            return f"Error generating response: {e}"

    def _generate_transformers(self, user_prompt: str) -> str:
        """Generate response using transformers."""
        # Combine system prompt with user prompt
        full_prompt = f"{self.system_prompt}\n\nUser: {user_prompt}\n\nAssistant:"

        try:
            # Generate response
            result = self.generator(full_prompt, return_full_text=False)
            response = result[0]["generated_text"].strip()
            return response
        except Exception as e:
            return f"Error generating response: {e}"

    def run_interactive_loop(self):
        """Run the interactive prompt loop."""
        session = PromptSession(history=InMemoryHistory())

        self.console.print("[bold cyan]Interactive Mode[/bold cyan]")
        self.console.print("Type your prompts below. Press Ctrl+D or type 'exit' to quit.\n")

        while True:
            try:
                # Get user input
                user_input = session.prompt("You: ")

                if user_input.lower() in ["exit", "quit"]:
                    self.console.print("\n[bold yellow]Goodbye![/bold yellow]")
                    break

                if not user_input.strip():
                    continue

                # Generate LLM response
                self.console.print("\n[bold magenta]Generating response...[/bold magenta]")
                llm_output = self.generate_response(user_input)

                # Display LLM output
                self.console.print("\n[bold green]LLM Output:[/bold green]")
                self.console.print(Panel(llm_output, border_style="green"))

                # Process through backend
                backend_output = self.backend.process(llm_output)

                # Display backend output with syntax highlighting
                self.console.print("\n[bold blue]Backend Output:[/bold blue]")
                syntax = Syntax(
                    backend_output,
                    self.backend.output_language,
                    theme="monokai",
                    line_numbers=True
                )
                self.console.print(Panel(syntax, border_style="blue"))
                self.console.print()

            except KeyboardInterrupt:
                self.console.print("\n[bold yellow]Interrupted[/bold yellow]")
                continue
            except EOFError:
                self.console.print("\n[bold yellow]Goodbye![/bold yellow]")
                break


def load_system_prompt(prompt_file: Path) -> str:
    """Load system prompt from a file."""
    try:
        with open(prompt_file, "r", encoding="utf-8") as f:
            return f.read().strip()
    except FileNotFoundError:
        print(f"Error: System prompt file '{prompt_file}' not found.")
        sys.exit(1)
    except Exception as e:
        print(f"Error reading system prompt file: {e}")
        sys.exit(1)


def run_server(llm_interface: LLMInterface, host: str = "0.0.0.0", port: int = 8000):
    """Start a FastAPI server to expose the LLM interface."""
    try:
        from fastapi import FastAPI, HTTPException
        from pydantic import BaseModel
        import uvicorn
    except ImportError:
        print("Error: FastAPI and uvicorn are required for server mode.")
        print("Install with: pip install fastapi uvicorn")
        sys.exit(1)

    app = FastAPI(
        title="GuideRail LLM API",
        description="API for querying LLM with backend processing",
        version="1.0.0"
    )

    class QueryRequest(BaseModel):
        prompt: str
        include_backend_output: bool = True

    class QueryResponse(BaseModel):
        llm_output: str
        backend_output: Optional[str] = None

    @app.post("/query", response_model=QueryResponse)
    async def query_llm(request: QueryRequest):
        """
        Query the LLM with a prompt and optionally get backend-processed output.
        """
        if not request.prompt.strip():
            raise HTTPException(status_code=400, detail="Prompt cannot be empty")

        try:
            # Generate LLM response
            llm_output = llm_interface.generate_response(request.prompt)

            # Process through backend if requested
            backend_output = None
            if request.include_backend_output:
                backend_output = llm_interface.backend.process(llm_output)

            return QueryResponse(
                llm_output=llm_output,
                backend_output=backend_output
            )
        except Exception as e:
            raise HTTPException(status_code=500, detail=f"Error processing request: {str(e)}")

    @app.get("/health")
    async def health_check():
        """Health check endpoint."""
        return {
            "status": "healthy",
            "model": llm_interface.model_name,
            "backend": llm_interface.backend.__class__.__name__
        }

    @app.get("/")
    async def root():
        """Root endpoint with API information."""
        return {
            "message": "GuideRail LLM API",
            "endpoints": {
                "POST /query": "Query the LLM",
                "GET /health": "Health check",
                "GET /docs": "API documentation"
            }
        }

    # Start the server
    llm_interface.console.print(f"\n[bold green]🚀 Starting FastAPI server on {host}:{port}[/bold green]")
    llm_interface.console.print(f"[cyan]API Documentation: http://{host if host != '0.0.0.0' else 'localhost'}:{port}/docs[/cyan]")
    llm_interface.console.print(f"[cyan]Health Check: http://{host if host != '0.0.0.0' else 'localhost'}:{port}/health[/cyan]\n")

    uvicorn.run(app, host=host, port=port)


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description="Interactive CLI for HuggingFace LLMs with backend processing"
    )
    parser.add_argument(
        "--model",
        type=str,
        default="Intel/Qwen3-Coder-30B-A3B-Instruct-gguf-q4km-AutoRound",
        help="HuggingFace model name (default: Intel/Qwen3-Coder-30B-A3B-Instruct-gguf-q4km-AutoRound)",
    )
    parser.add_argument(
        "--system-prompt",
        type=Path,
        default="system.txt",
        help="Path to system prompt text file (default: system.txt)"
    )
    parser.add_argument(
        "--backend",
        type=str,
        default="json",
        choices=list(BACKENDS.keys()),
        help="Backend processor to use (default: json)"
    )
    parser.add_argument(
        "--transformers",
        action="store_true",
        help="Use Transformers model format (requires transformers/torch)"
    )
    parser.add_argument(
        "--serve",
        action="store_true",
        help="Start FastAPI server instead of interactive mode"
    )
    parser.add_argument(
        "--host",
        type=str,
        default="0.0.0.0",
        help="Server host (default: 0.0.0.0)"
    )
    parser.add_argument(
        "--port",
        type=int,
        default=8000,
        help="Server port (default: 8000)"
    )

    args = parser.parse_args()
    system_prompt = load_system_prompt(args.system_prompt)

    backend_class = BACKENDS[args.backend]
    backend = backend_class()

    console = Console()

    llm_interface = LLMInterface(
        model_name=args.model,
        system_prompt=system_prompt,
        backend=backend,
        console=console,
        use_gguf=not args.transformers,
    )

    if args.serve:
        run_server(llm_interface, host=args.host, port=args.port)
    else:
        llm_interface.run_interactive_loop()


if __name__ == "__main__":
    main()
